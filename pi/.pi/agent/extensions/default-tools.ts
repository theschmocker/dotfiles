import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
	pi.on('session_start', () => {
		pi.setActiveTools(["ls", "grep", "find", "read", "bash", "edit", "write"]);
	});
}
