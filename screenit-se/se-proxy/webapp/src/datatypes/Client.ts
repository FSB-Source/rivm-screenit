import type {Adres} from "./Adres"
import type {TijdelijkAdres} from "./TijdelijkAdres"
import type {VorigOnderzoek, VorigOnderzoekDto} from "./VorigOnderzoek"
import {vorigOnderzoekDtoToVorigOnderzoek} from "./VorigOnderzoek"

export type Doelgroep = "REGULIER" | "DUBBELE_TIJD" | "MINDER_VALIDE";
export type ClientDto = {
	id: number;
	bsn: string;
	voorletters: string;
	geboorteTussenvoegsel: string;
	geboorteAchternaam: string;
	aanspreekTussenvoegselEnAchternaam: string;
	adres: Adres;
	tijdelijkGbaAdres?: Adres;
	tijdelijkAdres?: TijdelijkAdres;
	geboortedatum: string;
	geslacht: string;
	emailadres: string;
	telefoonnummer1: string;
	telefoonnummer2: string;
	doelgroep: Doelgroep;
	inTehuis: boolean;
	dubbeleTijdReden?: string;
	jaarLaatsteVerwijzing?: number;
	vorigeOnderzoeken: Array<VorigOnderzoekDto>;

};
export type Client = {
	id: number;
	bsn: string;
	voorletters: string;
	geboorteTussenvoegsel: string;
	geboorteAchternaam: string;
	aanspreekTussenvoegselEnAchternaam: string;
	adres: Adres;
	tijdelijkGbaAdres?: Adres;
	tijdelijkAdres?: TijdelijkAdres;
	geboortedatum: string;
	geslacht: string;
	emailadres: string;
	telefoonnummer1?: string;
	telefoonnummer2?: string;
	doelgroep: Doelgroep;
	inTehuis: boolean;
	dubbeleTijdReden?: string;
	jaarLaatsteVerwijzing?: number;
	vorigeOnderzoeken: Array<VorigOnderzoek>;

};

export function mapClientDtoToClient(clientDto: ClientDto): Client {
	return {
		id: clientDto.id,
		bsn: clientDto.bsn,
		voorletters: clientDto.voorletters,
		geboorteTussenvoegsel: clientDto.geboorteTussenvoegsel,
		geboorteAchternaam: clientDto.geboorteAchternaam,
		aanspreekTussenvoegselEnAchternaam: clientDto.aanspreekTussenvoegselEnAchternaam,
		adres: clientDto.adres,
		tijdelijkGbaAdres: clientDto.tijdelijkGbaAdres,
		tijdelijkAdres: clientDto.tijdelijkAdres,
		geboortedatum: clientDto.geboortedatum,
		geslacht: clientDto.geslacht,
		emailadres: clientDto.emailadres,
		telefoonnummer1: clientDto.telefoonnummer1,
		telefoonnummer2: clientDto.telefoonnummer2,
		doelgroep: clientDto.doelgroep,
		inTehuis: clientDto.inTehuis,
		dubbeleTijdReden: clientDto.dubbeleTijdReden,
		jaarLaatsteVerwijzing: clientDto.jaarLaatsteVerwijzing,
		vorigeOnderzoeken: maakVorigOnderzoeken(clientDto),
	}
}

const maakVorigOnderzoeken = (clientDto: ClientDto): Array<VorigOnderzoek> => {
	const result: Array<VorigOnderzoek> = []

	if (clientDto.vorigeOnderzoeken) {
		clientDto.vorigeOnderzoeken.forEach((vorigOnderzoekDto) => {
			const vorigOnderzoek = vorigOnderzoekDtoToVorigOnderzoek(vorigOnderzoekDto)

			if (vorigOnderzoek) {
				result.push(vorigOnderzoek)
			}
		})
	}

	return result
}