import type {ErrorDto} from "../datatypes/ErrorDto"

export type ErrorActions = FatalErrorAction;
export const FATAL_ERROR = "FATAL_ERROR"
export type FatalErrorAction = {
	type: "FATAL_ERROR";
	error: ErrorDto;
};
export const createActionFatalError = (error: ErrorDto): FatalErrorAction => ({
	type: FATAL_ERROR,
	error: error,
})