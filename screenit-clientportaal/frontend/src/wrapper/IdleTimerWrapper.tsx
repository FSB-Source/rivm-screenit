/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import IdleTimer from "react-idle-timer"
import {useDispatch, useSelector} from "react-redux"
import {State} from "../datatypes/State"
import {setSessionExpiredAction} from "../actions/AuthenticatieAction"

const IdleTimerWrapper = (props: { children: JSX.Element }) => {
	const isLoggedIn = useSelector((state: State) => state.authenticatie.isLoggedIn)
	const dispatch = useDispatch()

	const onIdle = () => {
		if (isLoggedIn) {
			dispatch(setSessionExpiredAction(true))
		}
	}
	return (
		<IdleTimer
			timeout={15 * 60 * 1000}
			onIdle={onIdle}>
			{props.children}
		</IdleTimer>
	)
}

export default IdleTimerWrapper
