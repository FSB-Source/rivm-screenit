export type ErrorType = "verplicht" | "getal" | "ongeldig";
export type ValidationError = {
	type: ErrorType;
	label: string;
};

const ongeldigErrorMessage = (label: string): string => {
	return `${label} is niet geldig.`
}

const requiredMessage = (label: string): string => {
	return `${label} is verplicht.`
}

const numberErrorMessage = (label: string): string => {
	return `${label} moet een heel getal zijn.`
}

export const getErrorMessage = (error: ValidationError): string => {
	switch (error.type) {
		case "verplicht":
			return requiredMessage(error.label)
		case "getal":
			return numberErrorMessage(error.label)
		case "ongeldig":
			return ongeldigErrorMessage(error.label)
		default:
			return ""
	}
}