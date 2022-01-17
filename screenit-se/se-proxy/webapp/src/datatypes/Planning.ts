export type Tijdslot = {
	vanafDatum: string;
	vanafTijd: string;
}

export class GeenScreeningBlok implements Tijdslot {
	vanafDatum: string
	vanafTijd: string
	totDatumTijd: string
	opmerking: string

	constructor(vanafDatum: string, vanafTijd: string, totDatumTijd: string, opmerking: string) {
		this.vanafDatum = vanafDatum
		this.vanafTijd = vanafTijd
		this.totDatumTijd = totDatumTijd
		this.opmerking = opmerking
	}
}

export type GeenScreeningBlokDto = {
	vanaf: string;
	tot: string;
	opmerking: string;
};

export type Planning = {
	geenScreeningBlokken: Array<GeenScreeningBlok>;
};

export type PlanningDto = {
	geenScreeningBlokken: Array<GeenScreeningBlokDto>;
};