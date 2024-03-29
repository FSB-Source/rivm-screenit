export type AnnotatieIcoonDto = {
	positieX: number;
	positieY: number;
	tekst?: string;
	type: AnnotatieIcoonType;
};
export type AnnotatieIcoon = {
	icoonId: number;
	positieX: number;
	positieY: number;
	tekst?: string;
	type: AnnotatieIcoonType;
	nieuwIcoon: boolean;
};
export type AnnotatieIcoonType =
	"AMPUTATIE"
	| "BORSTVERGROTING"
	| "DUBBELZIJDIGE_BORSTVERKLEINING"
	| "EENZIJDIGE_BORSTVERKLEINING"
	| "GROTER_DAN"
	| "INGETROKKEN_TEPEL"
	| "KLEINER_DAN"
	| "LITTEKEN_HORIZONTAAL"
	| "LITTEKEN_LBRO"
	| "LITTEKEN_RBLO"
	| "LITTEKEN_VERTICAAL"
	| "UITWENDIGE_AFWIJKING"
	| "WRAT"
	| "SIGNALERING_ARCHITECTUURVERSTORING"
	| "SIGNALERING_ASYMMETRIE"
	| "SIGNALERING_CALCIFICATIES"
	| "SIGNALERING_MASSA";

export type LegacyIcoonType =
	"LEGACY_PLUS"
	| "LEGACY_ARCHITECTUURVERSTORING_MET_CALCIFICATIES"
	| "LEGACY_MASSA_MET_ARCHITECTUURVERSTORING"
	| "LEGACY_CONFORM"
	| "LEGACY_GEEN_BIJZONDERHEDEN"
	| "LEGACY_MASSA_MET_SPICULAE"
	| "LEGACY_PROJECTIE_NAAR_LINKS"
	| "LEGACY_PROJECTIE_NAAR_RECHTS"
	| "LEGACY_MASSA_MET_CALCIFICATIES"
	| "LEGACY_MASSA_MET_SPICULAE_EN_CALCIFICATIES"
	| "LEGACY_MARKERING"
	| "LEGACY_BENIGNE_KALK";