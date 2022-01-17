import {RechtMetGeldigheid} from "./RechtMetGeldigheid"

export type AutorisatieDto = {
	displayName: string;
	username: string;
	medewerkercode: string;
	seCode: string;
	seNaam: string;
	instellingGebruikerId: number;
	navigatie: string;
} & RechtMetGeldigheid