import {FhirPatient} from "./FhirPatient"

export type FhirContext = {
	type: string;
	value: string;
	worklist?: FhirPatient[];
};