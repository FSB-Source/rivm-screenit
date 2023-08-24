/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {useEffect, useState} from "react"
import {useKeycloak} from "@react-keycloak/web"
import {useLocation} from "react-router-dom"
import {Bevolkingsonderzoek} from "../datatypes/Bevolkingsonderzoek"
import {useDispatch, useSelector} from "react-redux"
import {State} from "../datatypes/State"
import {createPersoonAction} from "../actions/PersoonAction"
import ScreenitBackend from "../utils/Backend"
import {useThunkDispatch} from "../index"
import {getRegio} from "../api/RegioThunkAction"
import {getBeschikbareContactActies} from "../api/ContactActiesThunkAction"
import {assertUnreachable} from "./EnumUtil"
import {refreshCervixDossier, refreshColonDossier, refreshMammaDossier} from "../api/RefreshDossierActions"
import {createSetEnvironmentInfoAction} from "../actions/EnvironmentInfoAction"
import {createHideAllToastsAction} from "../actions/ToastAction"

export const useSelectedBvo = (): Bevolkingsonderzoek | undefined => {
	const location = useLocation()

	if (location.pathname.startsWith("/mamma")) {
		return Bevolkingsonderzoek.MAMMA
	}
	if (location.pathname.startsWith("/cervix")) {
		return Bevolkingsonderzoek.CERVIX
	}
	if (location.pathname.startsWith("/colon")) {
		return Bevolkingsonderzoek.COLON
	}
}

export const useRefreshEnvironmentInfo = () => {
	const dispatch = useDispatch()
	const {keycloak} = useKeycloak()

	useEffect(() => {
		if (keycloak.authenticated) {
			ScreenitBackend.get("/environment").then(result => {
				dispatch(createSetEnvironmentInfoAction(result.data))
			})
		}
	}, [dispatch, keycloak.authenticated])
}

export const useRefreshClient = () => {
	const dispatch = useThunkDispatch()
	const authenticatie = useSelector((state: State) => state.authenticatie)
	const {keycloak} = useKeycloak()
	const location = useLocation()

	useEffect(() => {
		if (keycloak.authenticated && authenticatie.isLoggedIn && !authenticatie.isLoggingOut) {
			ScreenitBackend.get("/persoon").then(result => {
				dispatch(createPersoonAction(result.data))
				dispatch(getBeschikbareContactActies())
			})
		}
	}, [location.pathname, authenticatie, dispatch, keycloak])
}

export const useRefreshBvoDossier = () => {
	const selectedBvo = useSelectedBvo()

	const isLoggedIn = useSelector((state: State) => state.authenticatie.isLoggedIn)
	const dispatch = useThunkDispatch()
	const {keycloak} = useKeycloak()

	const pathname = useLocation().pathname

	useEffect(() => {
		if (keycloak.authenticated && isLoggedIn && selectedBvo) {
			switch (selectedBvo) {
				case Bevolkingsonderzoek.CERVIX:
					dispatch(refreshCervixDossier())
					break
				case Bevolkingsonderzoek.COLON:
					dispatch(refreshColonDossier())
					break
				case Bevolkingsonderzoek.MAMMA:
					dispatch(refreshMammaDossier())
					break
				default:
					assertUnreachable(selectedBvo)

			}
		}
	}, [pathname, selectedBvo, dispatch, isLoggedIn, keycloak.authenticated])
}

export const useRegio = () => {
	const regio = useSelector((state: State) => state.client.regio)
	const authenticatie = useSelector((state: State) => state.authenticatie)
	const thunkDispatch = useThunkDispatch()
	const {keycloak} = useKeycloak()

	useEffect(() => {
		if (keycloak.authenticated && authenticatie.isLoggedIn && !authenticatie.isLoggingOut && !regio) {
			thunkDispatch(getRegio())
		}
	}, [regio, authenticatie, thunkDispatch, keycloak])

	return regio
}

export const useRedirectAfterSeconds = (url: string, timeoutSeconds: number) => {
	useEffect(() => {
		const timeout = setTimeout(() => {
			window.location.replace(url)
		}, timeoutSeconds * 1000)

		return () => {
			timeout && clearTimeout(timeout)
		}
	}, [timeoutSeconds, url])
}

export const useHideAllToasts = () => {
	const dispatch = useDispatch()
	const location = useLocation()
	const {pathname} = location
	useEffect(() => {
		dispatch(createHideAllToastsAction())
	}, [pathname, dispatch])
}

export const useGotoPageTop = () => {
	const location = useLocation()
	useEffect(() => {
		window.scrollTo(0, 0)
	}, [location.pathname])
}

export type WindowDimensions = {
	width: number;
	height: number;
}

function getWindowDimensions(): WindowDimensions {
	const {innerWidth: width, innerHeight: height} = window
	return {
		width,
		height,
	}
}

export const useWindowDimensions = (): WindowDimensions => {
	const [windowDimensions, setWindowDimensions] = useState(getWindowDimensions())
	useEffect(() => {
		function handleResize() {
			setWindowDimensions(getWindowDimensions())
		}

		window.addEventListener("resize", handleResize)
		return () => window.removeEventListener("resize", handleResize)
	}, [])
	return windowDimensions
}
