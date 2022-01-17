import type {SeAction} from "../actions/SeAction"
import {Dispatch} from "redux"

export const dispatchActions = (dispatch: Dispatch, ...actions: Array<SeAction>): void => {
	actions.forEach(action => {
		dispatch(action)
	})
}