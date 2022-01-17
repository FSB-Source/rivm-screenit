export type SuboptimaleInsteltechniek = "FYSIEK_BEPERKT" | "MOBIEL_BEPERKT" | "MOEILIJK_TE_POSITIONEREN";

export function getSuboptimaleInsteltechniekBeschrijving(techniek: SuboptimaleInsteltechniek): string {
	switch (techniek) {
		case "FYSIEK_BEPERKT":
			return "Fysiek beperkt"
		case "MOBIEL_BEPERKT":
			return "Mobiel beperkt"
		case "MOEILIJK_TE_POSITIONEREN":
			return "Moeilijk te positioneren"
		default:
			return ""
	}
}
