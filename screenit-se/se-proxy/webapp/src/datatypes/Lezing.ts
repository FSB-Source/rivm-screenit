import type {BiradsWaarde} from "./BiradsWaarde"
import type {DoorsnedeAfbeeldingen, DoorsnedeAfbeeldingenDto} from "./DoorsnedeAfbeeldingen"
import {mapDoorsnedeAfbeeldingenFromDto} from "./DoorsnedeAfbeeldingen"
import type {VorigOnderzoek} from "./VorigOnderzoek"

export type LezingDto = {
	radioloogNaam: string;
	lezingType: string;
	biradsRechts: BiradsWaarde;
	biradsLinks: BiradsWaarde;
	lezingAanzichten: DoorsnedeAfbeeldingenDto;
};
export type Lezing = {
	radioloogNaam: string;
	lezingType: string;
	biradsRechts: BiradsWaarde;
	biradsLinks: BiradsWaarde;
	lezingAanzichten?: DoorsnedeAfbeeldingen;
	vorigOnderzoek: VorigOnderzoek;
};

export function mapLezingDtoToLezing(lezingDto: LezingDto, vorigOnderzoek: VorigOnderzoek): Lezing {
	return {
		radioloogNaam: lezingDto.radioloogNaam,
		lezingType: lezingDto.lezingType,
		biradsRechts: lezingDto.biradsRechts,
		biradsLinks: lezingDto.biradsLinks,
		lezingAanzichten: lezingDto.lezingAanzichten ? mapDoorsnedeAfbeeldingenFromDto(lezingDto.lezingAanzichten, 0) : undefined,
		vorigOnderzoek,
	}
}