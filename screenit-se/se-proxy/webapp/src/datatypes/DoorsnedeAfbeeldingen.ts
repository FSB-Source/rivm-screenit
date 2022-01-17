import type {AnnotatieAfbeelding, AnnotatieAfbeeldingDto} from "./AnnotatieAfbeelding"
import {mapAfbeeldingDtoToAfbeelding, mapAfbeeldingToIconenArrayDto} from "./AnnotatieAfbeelding"

export type DoorsnedeAfbeeldingen = {
	rechtsVerticaleDoorsnede?: AnnotatieAfbeelding;
	linksVerticaleDoorsnede?: AnnotatieAfbeelding;
	rechtsHorizontaleDoorsnede?: AnnotatieAfbeelding;
	linksHorizontaleDoorsnede?: AnnotatieAfbeelding;
};
export const newDoorsnedeAfbeeldingen = (): DoorsnedeAfbeeldingen => {
	return {
		rechtsVerticaleDoorsnede: undefined,
		linksVerticaleDoorsnede: undefined,
		rechtsHorizontaleDoorsnede: undefined,
		linksHorizontaleDoorsnede: undefined,
	}
}

export type DoorsnedeAfbeeldingenDto = {
	rechtsVerticaleDoorsnede?: AnnotatieAfbeeldingDto;
	linksVerticaleDoorsnede?: AnnotatieAfbeeldingDto;
	rechtsHorizontaleDoorsnede?: AnnotatieAfbeeldingDto;
	linksHorizontaleDoorsnede?: AnnotatieAfbeeldingDto;
};

export const mapDoorsnedeAfbeeldingenFromDto = (doorsnedeAfbeeldingenDto: DoorsnedeAfbeeldingenDto, afspraakId: number): DoorsnedeAfbeeldingen => {
	return {
		linksVerticaleDoorsnede: mapAfbeeldingDtoToAfbeelding(afspraakId, doorsnedeAfbeeldingenDto.linksVerticaleDoorsnede),
		rechtsVerticaleDoorsnede: mapAfbeeldingDtoToAfbeelding(afspraakId, doorsnedeAfbeeldingenDto.rechtsVerticaleDoorsnede),
		linksHorizontaleDoorsnede: mapAfbeeldingDtoToAfbeelding(afspraakId, doorsnedeAfbeeldingenDto.linksHorizontaleDoorsnede),
		rechtsHorizontaleDoorsnede: mapAfbeeldingDtoToAfbeelding(afspraakId, doorsnedeAfbeeldingenDto.rechtsHorizontaleDoorsnede),
	}
}

export const mapDoorsnedeAfbeeldingenToDoorsnedeAfbeeldingenDto = (doorsnedeAfbeeldingen: DoorsnedeAfbeeldingen): DoorsnedeAfbeeldingenDto => {
	return {
		linksHorizontaleDoorsnede: {
			iconen: doorsnedeAfbeeldingen.linksHorizontaleDoorsnede ? mapAfbeeldingToIconenArrayDto(doorsnedeAfbeeldingen.linksHorizontaleDoorsnede) : [],
		},
		rechtsHorizontaleDoorsnede: {
			iconen: doorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede ? mapAfbeeldingToIconenArrayDto(doorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede) : [],
		},
		linksVerticaleDoorsnede: {
			iconen: doorsnedeAfbeeldingen.linksVerticaleDoorsnede ? mapAfbeeldingToIconenArrayDto(doorsnedeAfbeeldingen.linksVerticaleDoorsnede) : [],
		},
		rechtsVerticaleDoorsnede: {
			iconen: doorsnedeAfbeeldingen.rechtsVerticaleDoorsnede ? mapAfbeeldingToIconenArrayDto(doorsnedeAfbeeldingen.rechtsVerticaleDoorsnede) : [],
		},
	}
}