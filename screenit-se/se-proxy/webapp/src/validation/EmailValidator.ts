import type {ValidationError} from "../datatypes/Error"
import {getErrorMessage} from "../datatypes/Error"
import type {Validation} from "./Validation"

export class EmailValidator<T extends string | undefined> implements Validation<T> {
	isValid(value: string | undefined): boolean {
		return isEmailadresValideOfLeeg(value)
	}

	getErrorMessage(value: string | undefined, fieldLabel: string): string {
		const errors: ValidationError[] = validateEmailadres(value, fieldLabel)
		if (errors.length > 0) {
			return getErrorMessage(errors[0])
		}
		return ""
	}
}

function isEmailadresValideOfLeeg(email: string | undefined, verplicht = false): boolean {
	if (!email) {
		return !verplicht
	}
	return email.length <= 100 && /^[_A-Za-z0-9-]+(\.[_A-Za-z0-9-]+)*@[a-zA-Z0-9]+([.-][a-zA-Z0-9]+)*\.[a-zA-Z]{2,}$/.test(email)
}

function validateEmailadres(email: string | undefined, label: string): ValidationError[] {
	const errors: ValidationError[] = []
	validateEmailadresOngeldigError(email, label, errors)
	return errors
}

function validateEmailadresOngeldigError(email: string | undefined, label: string, errors: ValidationError[]): void {
	if (!isEmailadresValideOfLeeg(email)) {
		errors.push({
			type: "ongeldig",
			label: label,
		})
	}
}
