import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
	pi.on('session_start', () => {
		pi.setActiveTools(["ls", "grep", "find", "read", "bash", "edit", "write"]);
	});
}
