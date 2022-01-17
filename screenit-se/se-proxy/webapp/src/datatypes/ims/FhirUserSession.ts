import type {FhirUser} from "./FhirUser"
import type {FhirContext} from "./FhirContext"
import type {FhirFocus} from "./FhirFocus"

export type FhirUserSession = {
	resourceType: string;
	user: FhirUser;
	context: FhirContext;
	focus?: FhirFocus;
};