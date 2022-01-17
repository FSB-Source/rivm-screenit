import type {AnnotatieIcoonType} from "./AnnotatieIcoon"

export type IcoonAfbeelding = {
	afbeelding: string;
	width: number;
	height: number;
	isRightUpperCornerOrigin?: boolean;
};

export const iconenLijst: Array<AnnotatieIcoonType> = ["EENZIJDIGE_BORSTVERKLEINING", "DUBBELZIJDIGE_BORSTVERKLEINING", "BORSTVERGROTING", "UITWENDIGE_AFWIJKING", "INGETROKKEN_TEPEL", "GROTER_DAN", "KLEINER_DAN", "AMPUTATIE", "LITTEKEN_RBLO", "LITTEKEN_VERTICAAL", "LITTEKEN_LBRO", "LITTEKEN_HORIZONTAAL", "WRAT"]