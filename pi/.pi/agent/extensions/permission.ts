import type {
	ExtensionAPI,
	ToolCallEvent,
	BashToolCallEvent,
	EditToolCallEvent,
	WriteToolCallEvent,
	ExtensionContext,
} from "@earendil-works/pi-coding-agent";
import { matchesKey, Key, truncateToWidth } from "@earendil-works/pi-tui";
import * as fs from "node:fs";
import * as path from "node:path";
import * as os from "node:os";

// ---------------------------------------------------------------------------
// State
// ---------------------------------------------------------------------------

interface PermissionState {
	editFiles: string[];
	writeFiles: string[];
	bashCommands: string[];
}

const DEFAULT_STATE: PermissionState = {
	editFiles: [],
	writeFiles: [],
	bashCommands: [],
};

const ALLOWLIST_TYPE = "permission-allowlist";

// Sensitive directories that block new-file bypass
const SENSITIVE_DIRS = [
	".git",
	"node_modules",
	".pi",
	".svn",
	".hg",
	".DS_Store",
];

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function normalizeToAbsolute(filePath: string, cwd: string): string {
	// Handle ~ prefix
	if (filePath.startsWith("~")) {
		filePath = filePath.replace(/^~/, os.homedir());
	}
	// Resolve to absolute path
	return path.resolve(cwd, filePath);
}

function normalizeToRelative(filePath: string, cwd: string): string {
	// Resolve to absolute, then back to relative from cwd
	const absolute = path.resolve(cwd, filePath);
	const relative = path.relative(cwd, absolute);
	// Normalize: remove leading ./, ensure forward slashes
	return relative.replace(/^\.\//, "").replace(/\\/g, "/");
}

function isSensitivePath(filePath: string): boolean {
	const parts = filePath.split("/");
	return parts.some((part) => SENSITIVE_DIRS.includes(part));
}

// ---------------------------------------------------------------------------
// Session state management
// ---------------------------------------------------------------------------

function restoreFromBranch(state: PermissionState, ctx: {
	sessionManager: { getBranch: () => Array<{ type: string; customType?: string; data?: unknown }> };
}): void {
	const branchEntries = ctx.sessionManager.getBranch();
	let lastAllowlistEntry: PermissionState | undefined;

	for (const entry of branchEntries) {
		if (entry.type === "custom" && entry.customType === ALLOWLIST_TYPE) {
			lastAllowlistEntry = entry.data as PermissionState | undefined;
		}
	}

	if (lastAllowlistEntry) {
		state.editFiles = lastAllowlistEntry.editFiles ?? [];
		state.writeFiles = lastAllowlistEntry.writeFiles ?? [];
		state.bashCommands = lastAllowlistEntry.bashCommands ?? [];
	}
}

function persistState(state: PermissionState, pi: ExtensionAPI): void {
	pi.appendEntry<PermissionState>(ALLOWLIST_TYPE, {
		editFiles: [...state.editFiles],
		writeFiles: [...state.writeFiles],
		bashCommands: [...state.bashCommands],
	});
}

function isInAllowlist(state: PermissionState, tool: string, target: string): boolean {
	switch (tool) {
		case "edit":
			return state.editFiles.includes(target);
		case "write":
			return state.writeFiles.includes(target);
		case "bash":
			return state.bashCommands.includes(target);
		default:
			return false;
	}
}

function addToAllowlist(state: PermissionState, tool: string, target: string): void {
	switch (tool) {
		case "edit":
			if (!state.editFiles.includes(target)) {
				state.editFiles.push(target);
			}
			// Also allow writes to the same file
			if (!state.writeFiles.includes(target)) {
				state.writeFiles.push(target);
			}
			break;
		case "write":
			if (!state.writeFiles.includes(target)) {
				state.writeFiles.push(target);
			}
			break;
		case "bash":
			if (!state.bashCommands.includes(target)) {
				state.bashCommands.push(target);
			}
			break;
	}
}

// ---------------------------------------------------------------------------
// Extension
// ---------------------------------------------------------------------------

export default function (pi: ExtensionAPI) {
	registerPermissionAllowlistCommand(pi);

	// In-memory state
	const state: PermissionState = { ...DEFAULT_STATE };

	// Restore on session start
	pi.on("session_start", async (_event, ctx) => {
		restoreFromBranch(state, ctx);
	});

	// Restore on tree navigation
	pi.on("session_tree", async (_event, ctx) => {
		restoreFromBranch(state, ctx);
	});

	// Clear on shutdown
	pi.on("session_shutdown", async () => {
		state.editFiles.length = 0;
		state.writeFiles.length = 0;
		state.bashCommands.length = 0;
	});

	// Permission gate for dangerous tools
	pi.on("tool_call", async (event, ctx) => {
		if (!REQUIRES_PERMISSION.includes(event.toolName)) {
			return undefined;
		}

		if (!ctx.hasUI) {
			return { block: true, reason: "Dangerous tool call blocked (no UI for confirmation)" };
		}

		// ---- write: new-file bypass ----
		if (isWriteTool(event)) {
			const absPath = normalizeToAbsolute(event.input.path, ctx.cwd);
			if (!fs.existsSync(absPath) && !isSensitivePath(absPath)) {
				// New file, not in sensitive dir — bypass and add to allowlist
				addToAllowlist(state, "write", absPath);
				addToAllowlist(state, "edit", absPath);
				persistState(state, pi);
				return undefined;
			}
		}

		// ---- Check allowlist ----
		const toolName = event.toolName;
		let target: string;

		if (isBashTool(event)) {
			target = event.input.command;
		} else if (isEditTool(event) || isWriteTool(event)) {
			target = normalizeToAbsolute(event.input.path, ctx.cwd);
		} else {
			target = "";
		}

		if (isInAllowlist(state, toolName, target)) {
			return undefined;
		}

		// Auto expand tool calls during confirmation, then restore later
		const wasExpanded = ctx.ui.getToolsExpanded();
		if (!wasExpanded) {
			ctx.ui.setToolsExpanded(true);
		}

		let choice: PromptChoice;
		try {
			choice = await showPermissionPrompt(event, ctx, toolName, target);
		} finally {
			ctx.ui.setToolsExpanded(wasExpanded);
		}

		if (choice === "always") {
			addToAllowlist(state, toolName, target);
			persistState(state, pi);
			return undefined;
		}

		if (choice !== "yes") {
			let reason = "Blocked by user";
			if (toolName === "write" || toolName === 'edit') {
				reason = "Blocked by user. File state unchanged.";
			}
			return { block: true, reason };
		}

		return undefined;
	});
}

function registerPermissionAllowlistCommand(pi: ExtensionAPI) {
	pi.registerCommand("permission-allowlist", {
		description: "Show permission allowlist entries",
		handler: async (_args, ctx) => {
			const state: PermissionState = { ...DEFAULT_STATE };
			restoreFromBranch(state, ctx);

			await ctx.ui.custom((tui, theme, _kb, done) => {
				let selectedTool = 0; // 0=edit, 1=write, 2=bash

				const renderLines = (width: number): string[] => {
					const lines: string[] = [];
					const title = theme.fg("accent", theme.bold("Permission Allowlist"));
					lines.push(title);
					lines.push("");

					// Tool tabs
					const tools = ["edit", "write", "bash"];
					let tabLine = "  ";
					for (let i = 0; i < tools.length; i++) {
						const prefix = i === selectedTool ? "> " : "  ";
						const color = i === selectedTool ? "accent" : "muted";
						tabLine += prefix + theme.fg(color, tools[i]);
						if (i < tools.length - 1) tabLine += "   ";
					}
					lines.push(tabLine);
					lines.push(theme.fg("border", "─".repeat(Math.max(0, width - 4))));
					lines.push("");

					// Get current tool's entries
					let entries: string[];
					switch (selectedTool) {
						case 0: entries = state.editFiles; break;
						case 1: entries = state.writeFiles; break;
						case 2: entries = state.bashCommands; break;
						default: entries = [];
					}

					if (entries.length === 0) {
						lines.push(theme.fg("muted", "  No entries"));
					} else {
						const maxWidth = Math.max(0, width - 4);
						for (const entry of entries) {
							let display = entry;
							// Truncate bash commands for display
							if (selectedTool === 2 && entry.length > 60) {
								display = entry.slice(0, 60) + "…";
							}
							lines.push("  " + truncateToWidth(display, maxWidth));
						}
					}

					lines.push("");
					lines.push(theme.fg("dim", "←→ switch tool • esc close"));

					return lines;
				};

				const component = {
					render: renderLines,
					invalidate() {},
					handleInput(data: string) {
						if (matchesKey(data, Key.left) || matchesKey(data, Key.right)) {
							if (matchesKey(data, Key.left) && selectedTool > 0) {
								selectedTool--;
							} else if (matchesKey(data, Key.right) && selectedTool < 2) {
								selectedTool++;
							}
							tui.requestRender();
						} else if (matchesKey(data, Key.escape)) {
							done(undefined);
						}
					},
				};

				return component;
			});
		},
	});
}

// ---------------------------------------------------------------------------
// Prompt display
// ---------------------------------------------------------------------------

type PromptChoice = "yes" | "no" | "always";

async function showPermissionPrompt(
	event: ToolCallEvent,
	ctx: ExtensionContext,
	toolName: string,
	target: string,
): Promise<PromptChoice> {
	let title = "⚠️ Dangerous tool call:\n\nAllow?";

	let displayTarget: string | null = null;
	if (isBashTool(event)) {
		const command = event.input.command;
		// Truncate very long commands for display
		const displayCmd = command.length > 120 ? command.slice(0, 120) + "…" : command;
		title = `⚠️ Dangerous command:\n\n  ${displayCmd}\n\nAllow?`;
	} else if (target) {
		displayTarget = normalizeToRelative(target, ctx.cwd)
		title = `⚠️ Dangerous tool call:\n\n  ${displayTarget}\n\nAllow?`;
	}

	const options = buildOptions(toolName, displayTarget ?? target);
	const choice = await ctx.ui.select(title, options);

	switch (choice) {
		case options[0]: return "yes";
		case options[2]: return "always";
		default: return "no";
	}
}

function buildOptions(toolName: string, target: string): string[] {
	switch (toolName) {
		case "edit":
			return ["Yes", "No", `Always allow edits to ${target}`];
		case "write":
			return ["Yes", "No", `Always allow writes to ${target}`];
		case "bash":
			return ["Yes", "No", "Always allow running this command (verbatim)"];
		default:
			return ["Yes", "No"];
	}
}

// ---------------------------------------------------------------------------
// Type guards
// ---------------------------------------------------------------------------

const REQUIRES_PERMISSION = ["bash", "edit", "write"];

function isBashTool(event: ToolCallEvent): event is BashToolCallEvent {
	return event.toolName === "bash";
}

function isEditTool(event: ToolCallEvent): event is EditToolCallEvent {
	return event.toolName === "edit";
}

function isWriteTool(event: ToolCallEvent): event is WriteToolCallEvent {
	return event.toolName === "write";
}
