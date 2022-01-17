import type {Identificatiesoort} from "../datatypes/Afspraak"
import type {ValidationError} from "../datatypes/Error"
import {getErrorMessage} from "../datatypes/Error"
import type {Validation} from "./Validation"

export type IdentificatieType = {
	identificatienummer?: string;
	identificatiesoort?: Identificatiesoort;
};

export class IdentificatieValidator<T extends IdentificatieType | undefined> implements Validation<T> {

	isValid(value?: IdentificatieType): boolean {
		if (value) {
			const errors = validateIdentificatieNummer(value.identificatienummer ? value.identificatienummer : "", value.identificatiesoort)
			return errors.length === 0
		}

		return false
	}

	getErrorMessage(value: IdentificatieType | undefined, fieldLabel: string): string {
		if (value) {
			const errors = validateIdentificatieNummer(value.identificatienummer ? value.identificatienummer : "", value.identificatiesoort)
			if (errors.length > 0) {
				return getErrorMessage(errors[0])
			}
		}
		return ""
	}

}

const identificatiesoortValidatieNaam = (soort: Identificatiesoort): string => {
	switch (soort) {
		case "PASPOORT":
			return "Nederlands paspoortnummer"
		case "RIJBEWIJS":
			return "Nederlands rijbewijsnummer"
		case "IDENTITEITSKAART":
			return "Nederlands identiteitskaartnummer"
		case "OVERIG":
			return "Overig tekst"
		default:
			return "Identificatienummer"
	}
}

const validateRequiredError = (value: string | undefined, errorLabel: string, errors: ValidationError[]): void => {
	if (!value || value.trim() === "") {
		errors.push({
			type: "verplicht",
			label: errorLabel,
		})
	}
}

const validateIdentificatieNummerOngeldigError = (identificatienummer: string | undefined, identificatiesoort: Identificatiesoort | undefined, errorLabel: string, errors: ValidationError[]): void => {
	let isValid = false
	switch (identificatiesoort) {
		case "PASPOORT":
		case "IDENTITEITSKAART":
			isValid = !!identificatienummer && /[A-NP-Za-np-z]{2}[A-NP-Za-np-z0-9]{6}[0-9]{1}/.test(identificatienummer)
			break
		case "RIJBEWIJS":
			isValid = !!identificatienummer && /[0-9]{10}/.test(identificatienummer)
			break
		case "OVERIG":
			isValid = true
			break
		default:
			break
	}

	if (!isValid) {
		errors.push({
			type: "ongeldig",
			label: errorLabel,
		})
	}
}

export function validateIdentificatieNummer(identificatienummer: string | undefined, identificatiesoort: Identificatiesoort | undefined): ValidationError[] {
	const errorLabel = identificatiesoort ? identificatiesoortValidatieNaam(identificatiesoort) : "Identificatienummer"
	const errors: ValidationError[] = []
	validateRequiredError(identificatienummer, errorLabel, errors)
	validateIdentificatieNummerOngeldigError(identificatienummer, identificatiesoort, errorLabel, errors)
	return errors
}

export function isValidIdentificatieNummer(identificatienummer: string | undefined, identificatiesoort: Identificatiesoort | undefined): boolean {
	return !!identificatiesoort && validateIdentificatieNummer(identificatienummer, identificatiesoort).length === 0
}