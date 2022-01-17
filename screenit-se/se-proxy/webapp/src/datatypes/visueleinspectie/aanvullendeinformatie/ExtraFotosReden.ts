export type ExtraFotosReden =
	"OMVANG_MAMMAE"
	| "TEPELPROJECTIE"
	| "KLIERWEEFSEL_NIET_VOLLEDIG_AFGEBEELD"
	| "INSTELTECHNISCHE_REDEN"
	| "INTERPRETATIE_KAN_PROBLEMEN_OPLEVEREN"
	| "ARCHITECTUUR_VERSTORING"
	| "ASYMMETRIE"
	| "BEWOGEN_FOTO"
	| "TECHNISCHE_REDEN"
	| "WETENSCHAPPELIJK_ONDERZOEK"
	| "OVERIGE_REDEN";

export const getExtraFotosRedenLabel = (option: ExtraFotosReden): string => {
	switch (option) {
		case "OMVANG_MAMMAE":
			return "Omvang mammae"
		case "TEPELPROJECTIE":
			return "Tepelprojectie"
		case "KLIERWEEFSEL_NIET_VOLLEDIG_AFGEBEELD":
			return "Klierweefsel niet volledig afgebeeld"
		case "INSTELTECHNISCHE_REDEN":
			return "Insteltechnische reden"
		case "INTERPRETATIE_KAN_PROBLEMEN_OPLEVEREN":
			return "Interpretatie kan problemen opleveren"
		case "ARCHITECTUUR_VERSTORING":
			return "Architectuur verstoring"
		case "ASYMMETRIE":
			return "Asymmetrie"
		case "BEWOGEN_FOTO":
			return "Bewogen foto"
		case "TECHNISCHE_REDEN":
			return "Technische reden"
		case "WETENSCHAPPELIJK_ONDERZOEK":
			return "Wetenschappelijk Onderzoek"
		case "OVERIGE_REDEN":
			return "Overige reden"
		default:
			return ""
	}
}
export const allExtraFotosRedenOptions: Array<ExtraFotosReden> = ["OMVANG_MAMMAE", "TEPELPROJECTIE", "KLIERWEEFSEL_NIET_VOLLEDIG_AFGEBEELD", "INSTELTECHNISCHE_REDEN", "INTERPRETATIE_KAN_PROBLEMEN_OPLEVEREN", "ARCHITECTUUR_VERSTORING", "ASYMMETRIE", "BEWOGEN_FOTO", "TECHNISCHE_REDEN", "WETENSCHAPPELIJK_ONDERZOEK", "OVERIGE_REDEN"]