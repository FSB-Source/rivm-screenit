export type OnderzoekType = "MAMMOGRAFIE" | "TOMOSYNTHESE"
export const MAMMOGRAFIE = "MAMMOGRAFIE"
export const TOMOSYNTHESE = "TOMOSYNTHESE"

export const alleOnderzoekTypes: Array<OnderzoekType> = [MAMMOGRAFIE, TOMOSYNTHESE]

export const getOnderzoekTypeLabel = (onderzoekType: OnderzoekType): string => {
	switch (onderzoekType) {
		case MAMMOGRAFIE:
			return "Mammografie"
		case TOMOSYNTHESE:
			return "Tomosynthese"
	}
}