import type {Validation} from "./Validation"

export type DubbeleTijdType = {
	dubbeleTijdReden?: string;
	dubbeleTijd: boolean;
};

export class DubbeleTijdRedenValidator<T extends DubbeleTijdType> implements Validation<T> {
	isValid(value: DubbeleTijdType | undefined): boolean {
		if (value && value.dubbeleTijd) {
			return !(!value.dubbeleTijdReden || value.dubbeleTijdReden.trim() === "")
		}

		return true
	}

	getErrorMessage(value: DubbeleTijdType | undefined, fieldLabel: string): string {
		return this.isValid(value) ? "" : `${fieldLabel} is verplicht.`
	}

}