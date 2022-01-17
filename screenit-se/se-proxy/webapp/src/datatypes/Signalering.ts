import type {DoorsnedeAfbeeldingen, DoorsnedeAfbeeldingenDto} from "./DoorsnedeAfbeeldingen"
import {mapDoorsnedeAfbeeldingenFromDto, mapDoorsnedeAfbeeldingenToDoorsnedeAfbeeldingenDto} from "./DoorsnedeAfbeeldingen"

export type Signalering = {
	heeftAfwijkingen: boolean;
	doorsnedeAfbeeldingen: DoorsnedeAfbeeldingen;
};

export type SignaleringDto = {
	heeftAfwijkingen: boolean;
	doorsnedeAfbeeldingen: DoorsnedeAfbeeldingenDto;
};

export const mapSignaleringToDto = (signalering: Signalering): SignaleringDto => {
	return {
		heeftAfwijkingen: signalering.heeftAfwijkingen,
		doorsnedeAfbeeldingen: mapDoorsnedeAfbeeldingenToDoorsnedeAfbeeldingenDto(signalering.doorsnedeAfbeeldingen),
	}
}

export const mapSignaleringFromDto = (signaleringDto: SignaleringDto, afspraakId: number): Signalering | undefined => {
	return signaleringDto ? {
		heeftAfwijkingen: signaleringDto.heeftAfwijkingen,
		doorsnedeAfbeeldingen: mapDoorsnedeAfbeeldingenFromDto(signaleringDto.doorsnedeAfbeeldingen, afspraakId),
	} : undefined
}