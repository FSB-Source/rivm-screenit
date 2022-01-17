import type {FhirCoding} from "./FhirCoding"

export type FhirIdentifier = {
	system: string;
	value: string | number;
	type?: FhirCoding;
};