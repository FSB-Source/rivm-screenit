import type {FhirPatient} from "./FhirPatient"
import type {FhirImagingStudy} from "./FhirImagingStudy"

export type FhirFocus = {
	patient: FhirPatient;
	imagingStudy: FhirImagingStudy;
};