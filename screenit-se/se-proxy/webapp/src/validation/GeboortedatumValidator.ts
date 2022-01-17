import type {ValidationError} from "../datatypes/Error"
import {getErrorMessage} from "../datatypes/Error"
import type {Validation} from "./Validation"
import moment from "moment"

export class GeboortedatumValidator<T extends string | undefined> implements Validation<T> {
	isValid(value: string | undefined): boolean {
		return isGeboortedatumValid(value)
	}

	getErrorMessage(value: string | undefined, fieldLabel: string): string {
		const errors: ValidationError[] = validateGeboortedatum(value, fieldLabel)

		if (errors.length > 0) {
			return getErrorMessage(errors[0])
		}

		return ""
	}

}

function isGeboortedatumValid(geboortedatum: string | undefined): boolean {
	if (geboortedatum) {
		if (geboortedatum.length === 10 && /[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}/.test(geboortedatum)) {
			const momentDate: moment.Moment = moment(geboortedatum, "YYYY-MM-DD")
			return momentDate.isAfter(moment().subtract(100, "years")) && momentDate.isBefore(moment())
		}
	}

	return false
}

function validateGeboortedatum(geboortedatum: string | undefined, label: string): ValidationError[] {
	const errors: ValidationError[] = []
	validateGeboortedatumOngeldigError(geboortedatum, label, errors)
	return errors
}

function validateGeboortedatumOngeldigError(geboortedatum: string | undefined, label: string, errors: ValidationError[]): void {
	if (!isGeboortedatumValid(geboortedatum)) {
		errors.push({
			type: "ongeldig",
			label: label,
		})
	}
}