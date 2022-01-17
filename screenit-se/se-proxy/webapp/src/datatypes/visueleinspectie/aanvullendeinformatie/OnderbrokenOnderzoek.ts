export type OnderbrokenOnderzoekOption = "GEEN_REDEN" | "TECHNISCHE_REDEN" | "CLIENT_ONWEL";

export const allOnderbrokenOnderzoekOptions: Array<OnderbrokenOnderzoekOption> = ["GEEN_REDEN", "TECHNISCHE_REDEN", "CLIENT_ONWEL"]

export const getOnderbrokenOnderzoekLabel = (option: OnderbrokenOnderzoekOption): string => {
	switch (option) {
		case "GEEN_REDEN":
			return "Geen reden"
		case "TECHNISCHE_REDEN":
			return "Technische reden"
		case "CLIENT_ONWEL":
			return "Cliënt onwel"
		default:
			return ""
	}
}