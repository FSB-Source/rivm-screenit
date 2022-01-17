export type RedenFotobespreking =
	"ARCHITECTUURVERSTORING"
	| "ASYMMETRIE"
	| "CALCIFICATIES"
	| "MASSA"
	| "INSTELTECHNIEK"
	| "OVERIG";

export function getRedenFotobesprekingString(redenFotobespreking: RedenFotobespreking): string {
	switch (redenFotobespreking) {
		case "ARCHITECTUURVERSTORING":
			return "Architectuurverstoring"
		case "ASYMMETRIE":
			return "Asymmetrie"
		case "CALCIFICATIES":
			return "Calcificaties"
		case "MASSA":
			return "Massa"
		case "INSTELTECHNIEK":
			return "Insteltechniek"
		case "OVERIG":
			return "Overig"
		default:
			return ""
	}
}
