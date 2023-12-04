export type Dagsynchronisatie = {
	gemaakt: number;
	verwerkt: number;
};

export type Dagafsluiting = {
	aantalDoorgevoerd: number;
};

export type MedewerkerDagproductie = {
	ingeschrevenCount?: number;
	onderzochtCount?: number;
	afgerondCount?: number;
	onderbrokenCount?: number;
	onvolledigCount?: number;
	afwijkingenCount?: number;
};

export type Dagproductie = {
	[medewerker: string]: MedewerkerDagproductie
};

export type DagPlanningSamenvatting = {
	dagCapaciteit: number;
	beschikbaarheid: number;
	starttijd?: string;
	eindtijd?: string;
	aantalVerwacht: number;
	aantalAfgerond: number;
	aantalOnderbroken: number;
	aantalOnvolledig: number;
}

export type Dagverslag = {
	dagSynchronisatie: Dagsynchronisatie;
	dagafsluiting: Dagafsluiting;
	dagproductie: Dagproductie;
	nietAfgeslotenVanaf?: string;
	dagPlanningSamenvatting: DagPlanningSamenvatting;
}