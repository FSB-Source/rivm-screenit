export type OnvolledigOnderzoekOption = "MET_FOTOS" | "ZONDER_FOTOS";

export const allOnvolledigOnderzoekOptions: Array<OnvolledigOnderzoekOption> = ["MET_FOTOS", "ZONDER_FOTOS"]

export const getOnvolledigOnderzoekLabel = (option: OnvolledigOnderzoekOption): string => {
	switch (option) {
		case "MET_FOTOS":
			return "Met foto's"
		case "ZONDER_FOTOS":
			return "Zonder foto's"
		default:
			return ""
	}
}