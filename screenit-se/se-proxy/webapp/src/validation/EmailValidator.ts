import type {ValidationError} from "../datatypes/Error"
import {getErrorMessage} from "../datatypes/Error"
import type {Validation} from "./Validation"

export class EmailValidator<T extends string | undefined> implements Validation<T> {
	isValid(value: string | undefined): boolean {
		return isEmailadresValid(value)
	}

	getErrorMessage(value: string | undefined, fieldLabel: string): string {
		const errors: ValidationError[] = validateEmaildres(value, fieldLabel)
		if (errors.length > 0) {
			return getErrorMessage(errors[0])
		}
		return ""
	}
}

function isEmailadresValid(email: string | undefined): boolean {
	if (email) {
		return /^(([a-zA-Z0-9_\-.]+)@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9-]+\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(]?)(\s*(;|,)\s*|\s*$))*$/.test(email)
	}
	return true
}

function validateEmaildres(email: string | undefined, label: string): ValidationError[] {
	const errors: ValidationError[] = []
	validateEmailadresOngeldigError(email, label, errors)
	return errors
}

function validateEmailadresOngeldigError(email: string | undefined, label: string, errors: ValidationError[]): void {
	if (!isEmailadresValid(email)) {
		errors.push({
			type: "ongeldig",
			label: label,
		})
	}
}