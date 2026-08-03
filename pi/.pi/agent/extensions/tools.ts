/**
 * Tools Extension
 *
 * Provides a /tools command to enable/disable tools interactively.
 * Tool selection persists across session reloads and respects branch navigation.
 *
 * Usage:
 * 1. Copy this file to ~/.pi/agent/extensions/ or your project's .pi/extensions/
 * 2. Use /tools to open the tool selector
 */

import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { getSettingsListTheme } from "@earendil-works/pi-coding-agent";
import { Container, type SettingItem, SettingsList } from "@earendil-works/pi-tui";

// State persisted to session
interface ToolsState {
	enabledTools: string[];
}

export default function toolsExtension(pi: ExtensionAPI) {
	// Track enabled tools
	let enabledTools: Set<string> = new Set();

	// Persist current state
	function persistState() {
		pi.appendEntry<ToolsState>("tools-config", {
			enabledTools: Array.from(enabledTools),
		});
	}

	// Apply current tool selection and update status bar
	function applyTools(ctx?: ExtensionContext) {
		pi.setActiveTools(Array.from(enabledTools));
		const active = Array.from(enabledTools);
		if (ctx) {
			if (active.length === 0) {
				ctx.ui.setStatus("tools", "no tools enabled");
			} else {
				ctx.ui.setStatus("tools", ctx.ui.theme.fg("dim", active.join(", ")));
			}
		}
	}

	function getSavedTools(ctx: ExtensionContext): string[] | null {
		// Get entries in current branch only
		const branchEntries = ctx.sessionManager.getBranch();
		let savedTools: string[] | null = null;

		for (const entry of branchEntries) {
			if (entry.type === "custom" && entry.customType === "tools-config") {
				const data = entry.data as ToolsState | undefined;
				if (data?.enabledTools) {
					savedTools = data.enabledTools;
				}
			}
		}

		return savedTools;
	}

	// Find the last tools-config entry in the current branch
	function restoreFromBranch(ctx: ExtensionContext) {
		const allTools = pi.getAllTools();

		// Get entries in current branch only
		const savedTools = getSavedTools(ctx);

		if (savedTools) {
			// Restore saved tool selection (filter to only tools that still exist)
			const allToolNames = allTools.map((t) => t.name);
			enabledTools = new Set(savedTools.filter((t: string) => allToolNames.includes(t)));
		} else {
			// No saved state - sync with currently active tools
			enabledTools = new Set(pi.getActiveTools());
		}
		applyTools(ctx);
	}

	async function selectTools(ctx: ExtensionContext) {
		await ctx.ui.custom((tui, theme, _kb, done) => {
			// Refresh tool list
			const allTools = pi.getAllTools();
			// Build settings items for each tool
			const items: SettingItem[] = allTools.map((tool) => ({
				id: tool.name,
				label: tool.name,
				currentValue: enabledTools.has(tool.name) ? "enabled" : "disabled",
				values: ["enabled", "disabled"],
			}));

			const container = new Container();
			container.addChild(
				new (class {
					render(_width: number) {
						return [theme.fg("accent", theme.bold("Tool Configuration")), ""];
					}
					invalidate() {}
				})(),
			);

			const settingsList = new SettingsList(
				items,
				Math.min(items.length + 2, 15),
				getSettingsListTheme(),
				(id, newValue) => {
					// Update enabled state and apply immediately
					if (newValue === "enabled") {
						enabledTools.add(id);
					} else {
						enabledTools.delete(id);
					}
					applyTools(ctx);
					persistState();
				},
				() => {
					// Close dialog
					done(undefined);
				},
			);

			container.addChild(settingsList);

			const component = {
				render(width: number) {
					return container.render(width);
				},
				invalidate() {
					container.invalidate();
				},
				handleInput(data: string) {
					settingsList.handleInput?.(data);
					tui.requestRender();
				},
			};

			return component;
		});
	}

	// Register /tools command
	pi.registerCommand("tools", {
		description: "Enable/disable tools",
		handler: async (_args, ctx) => {
			await selectTools(ctx);
		},
	});

	pi.registerShortcut('alt+t', {
		description: "Enable/disable tools",
		handler: async (ctx) => {
			await selectTools(ctx);
		},
	});

	// TODO: saved pre-fork tools and add a new entry to the new session
	// right now, tools will be restored to the state at the point of the
	// target message, rather than the most recent.

	// Restore state on session start
	pi.on("session_start", async (_event, ctx) => {
		restoreFromBranch(ctx);
	});

	// Restore state when navigating the session tree
	pi.on("session_tree", async (_event, ctx) => {
		restoreFromBranch(ctx);
	});
}
