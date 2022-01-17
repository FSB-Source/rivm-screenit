export type OpschortenReden = "NIET_OPSCHORTEN" | "AANVULLENDE_BEELDEN_NODIG_SE" | "PRIORS_VAN_BUITEN_BVO";
export const AANVULLENDE_BEELDEN_NODIG_SE = "AANVULLENDE_BEELDEN_NODIG_SE"

export function getOpschortenRedenBeschrijving(reden: OpschortenReden): string {
	switch (reden) {
		case "NIET_OPSCHORTEN":
			return "Niet opschorten"
		case "AANVULLENDE_BEELDEN_NODIG_SE":
			return "Aanvullende beelden nodig van de SE"
		case "PRIORS_VAN_BUITEN_BVO":
			return "Priors van buiten bevolkingsonderzoek nodig"
		default:
			return ""
	}
}
