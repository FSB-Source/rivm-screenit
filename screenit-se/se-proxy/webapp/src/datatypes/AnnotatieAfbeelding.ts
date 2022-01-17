import type {AnnotatieIcoon, AnnotatieIcoonDto} from "./AnnotatieIcoon"

let nextUnusedIcoonId = 1
export const getNextIcoonId = (): number => {
	return nextUnusedIcoonId++
}

export type AnnotatieAfbeeldingDto = {
	id?: number;
	iconen: Array<AnnotatieIcoonDto>;
};

export type AnnotatieAfbeelding = {
	afspraakId: number;
	iconenById: Map<number, AnnotatieIcoon>;
};

export const mapAfbeeldingToIconenArrayDto = (afbeelding: AnnotatieAfbeelding): Array<AnnotatieIcoonDto> => {
	if (!afbeelding.iconenById) {
		return []
	}

	return Array.from(afbeelding.iconenById.values()).map(value => {
		return {
			positieX: value.positieX,
			positieY: value.positieY,
			tekst: value.tekst,
			type: value.type,
		}
	})
}
export const mapAfbeeldingToEnkelDto = (afspraakId: number, afbeelding: AnnotatieAfbeelding): AnnotatieAfbeeldingDto => {
	return {
		id: afspraakId,
		iconen: mapAfbeeldingToIconenArrayDto(afbeelding),
	}
}

export const mapAfbeeldingDtoToAfbeelding = (afspraakId: number, afbeeldingDto: AnnotatieAfbeeldingDto | undefined): AnnotatieAfbeelding => {
	if (!afbeeldingDto || !afbeeldingDto.iconen) {
		return {
			afspraakId: afspraakId,
			iconenById: new Map(),
		}
	}

	return {
		afspraakId: afspraakId,
		iconenById: new Map(afbeeldingDto.iconen.map(i => {
			const id = getNextIcoonId()
			return [id, {
				icoonId: id,
				positieX: i.positieX,
				positieY: i.positieY,
				tekst: i.tekst,
				type: i.type,
				nieuwIcoon: false,
			}]
		})),
	}
}