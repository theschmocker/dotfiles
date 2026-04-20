import type {
	ExtensionAPI,
	ToolCallEvent,
	BashToolCallEvent,
} from "@mariozechner/pi-coding-agent";

const REQUIRES_PERMISSION = [
	"bash",
	"edit",
	"write",
];

// TODO: additional permission options, e.g. for edit tool, always allow edits to this file

export default function (pi: ExtensionAPI) {
	pi.on("tool_call", async (event, ctx) => {
		if (!REQUIRES_PERMISSION.includes(event.toolName)) {
			return undefined;
		}

		if (!ctx.hasUI) {
			// In non-interactive mode, block by default
			return { block: true, reason: "Dangerous command blocked (no UI for confirmation)" };
		}

		let confirmPrompt = "⚠️ Dangerous tool call:\n\nAllow?";
		if (isBashTool(event)) {
			const command = event.input.command as string || "";
			confirmPrompt = `⚠️ Dangerous command:\n\n  ${command}\n\nAllow?`
		}

		const choice = await ctx.ui.select(confirmPrompt, ["Yes", "No"]);

		if (choice !== "Yes") {
			return { block: true, reason: "Blocked by user" };
		}

		return undefined;
	});
}

function isBashTool(event: ToolCallEvent): event is BashToolCallEvent {
	return event.toolName === 'bash';
}
