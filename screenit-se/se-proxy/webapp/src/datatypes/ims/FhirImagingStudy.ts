import type {FhirIdentifier} from "./FhirIdentifier"

export type FhirImagingStudy = {
	accession: FhirIdentifier;
	identifier: FhirIdentifier;
};