import type {ValidationError} from "../datatypes/Error"
import {getErrorMessage} from "../datatypes/Error"
import type {Validation} from "./Validation"

export class BsnValidator<T extends string | undefined> implements Validation<T> {
	isValid(value: string | undefined): boolean {
		return isBsnValid(value)
	}

	getErrorMessage(value: string | undefined, fieldLabel: string): string {
		const errors: ValidationError[] = validateBsn(value, fieldLabel)
		if (errors.length > 0) {
			return getErrorMessage(errors[0])
		}
		return ""
	}
}

function isBsnValid(bsn: string | undefined): boolean {
	if (bsn && bsn.length === 9) {
		const check = parseInt(bsn[0], 10) * 9 + parseInt(bsn[1], 10) * 8 + parseInt(bsn[2], 10) * 7 + parseInt(bsn[3], 10) * 6 + parseInt(bsn[4], 10) * 5 + parseInt(bsn[5], 10) * 4 + parseInt(bsn[6], 10) * 3 + parseInt(bsn[7], 10) * 2 + parseInt(bsn[8], 10) * -1
		return check % 11 === 0
	}

	return false
}

function validateBsn(email: string | undefined, label: string): ValidationError[] {
	const errors: ValidationError[] = []
	validateBsnOngeldigError(email, label, errors)
	return errors
}

function validateBsnOngeldigError(bsn: string | undefined, label: string, errors: ValidationError[]): void {
	if (!isBsnValid(bsn)) {
		errors.push({
			type: "ongeldig",
			label: label,
		})
	}
}