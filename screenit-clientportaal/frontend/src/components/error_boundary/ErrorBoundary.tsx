/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import React, {Component, ReactNode} from "react"
import {Grid, IconButton} from "@mui/material"
import CloseIcon from "@mui/icons-material/Close"
import styles from "../../App.module.scss"
import supportedBrowsers from "../../supportedBrowsers"
import MijnBevolkingsOnderzoekLogo from "../../scss/media/MijnBevolkingsOnderzoekLogo"

interface Props {
	children?: ReactNode;
}

interface State {
	hasError: boolean;
}

class ErrorBoundary extends Component<Props, State> {
	public state: State = {
		hasError: false,
	}

	private isSupportedBrowser: boolean

	constructor(props: Props) {
		super(props)
		this.isSupportedBrowser = supportedBrowsers.test(navigator.userAgent)
	}

	static getDerivedStateFromError(error: Error) {
		return {hasError: true}
	}

	public render() {
		if (!this.state.hasError) {
			return this.props.children
		}

		return (
			<>
				<div className={styles.appErrorBoundary}>
					<Grid container justifyContent="flex-end">
						<IconButton href="https:
									aria-label="close">
							<CloseIcon/>
						</IconButton>
					</Grid>
					<Grid
						container
						spacing={0}
						alignItems="center"
						justifyContent="center"
						style={{minHeight: "80vh"}}
					>
						<Grid item xs={this.isSupportedBrowser ? 5 : 8}>
							<Grid item xs={10} justifyContent="center" alignItems="center" container style={{minHeight: "10vh"}}
							>
								<MijnBevolkingsOnderzoekLogo/>
							</Grid>
							{this.isSupportedBrowser
								?
								(
									<>
										<h1 data-testid="error_boundary_page_supported_browser">Er is iets misgegaan tijdens het weergeven van deze pagina</h1>
										Om technische redenen is het momenteel niet mogelijk deze pagina weer te geven. Excuses voor het ongemak. Probeer het later opnieuw.
									</>
								)
								:
								(
									<>
										<h1 data-testid="error_boundary_page_unsupported_browser">Helaas, het lukt niet om in te loggen in Mijn Bevolkingsonderzoek</h1>
										Dat komt omdat u gebruik maakt van een oudere webbrowser. Een webbrowser is het programma dat u gebruikt voor de toegang tot internet.
										Oudere browsers zijn minder veilig en werken niet precies hetzelfde als nieuwere versies. Wij raden u aan om uw browser te updaten.
										U kunt bijvoorbeeld gebruik maken van Google Chrome, Mozilla Firefox of Microsoft Edge.
									</>
								)
							}

						</Grid>
					</Grid>
				</div>
			</>
		)
	}
}

export default ErrorBoundary
