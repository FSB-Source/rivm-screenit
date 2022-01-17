import type {ClientDto} from "./Client"

export type PassantDto = {
	clientSeDto: ClientDto;
	afspraakVanaf?: string;
	afspraakSe?: string;
	uitnodigingsDatum?: string;
	eenmaligeAfmelding: boolean;
};