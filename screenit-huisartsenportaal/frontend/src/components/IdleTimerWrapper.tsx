/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */
import React from "react"
import {IdleTimerProvider} from "react-idle-timer"
import {useAppSelector, useAppThunkDispatch} from "../index"
import {loadingThunkAction} from "../api/LoadingThunkAction"
import {afmelden} from "../api/AfmeldenThunkAction"
import {createActionPushToast} from "../state/ToastsState"
import {ToastType} from "../state/datatypes/Toast"
import {useNavigate} from "react-router"

const IdleTimerWrapper = (props: { children: JSX.Element }) => {
	const dispatch = useAppThunkDispatch()
	const navigate = useNavigate()
	const auth = useAppSelector(state => state.oauth)

	const onIdle = () => {
		if (auth) {
			dispatch(loadingThunkAction(afmelden(auth))).then(() => {
				dispatch(createActionPushToast({type: ToastType.SUCCESS, message: "U bent uitgelogd"}))
				navigate("/login")
			})
		}
	}
	return (
		<IdleTimerProvider
			timeout={30 * 60 * 1000}
			onIdle={onIdle}
		>
			{props.children}
		</IdleTimerProvider>
	)
}

export default IdleTimerWrapper
