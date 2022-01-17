export interface Validation<T> {
	isValid(value: T): boolean;

	getErrorMessage(value: T, fieldLabel: string): string;
}