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
import React, {useMemo} from "react"
import supportedBrowsers from "../../../supportedBrowsers"
import {Alert, AlertTitle, Collapse} from "@mui/material"

const styles: React.CSSProperties = {
	backgroundColor: "#e3eaf1",
	color: "black",
	marginBottom: "10px",
}

const UnsupportedBrowserBanner: React.FC = () => {
	const isSupportedBrowser = useMemo(() => supportedBrowsers.test(navigator.userAgent), [])

	const [open, setOpen] = React.useState(true)

	if (isSupportedBrowser) {
		return null
	}

	return (
		<Collapse in={open}>
			<Alert severity="warning" data-testid="alert_banner_unsupported_browser" style={styles} onClose={(() => {
				setOpen(false)
			})}>
				<AlertTitle>Mogelijk werkt Mijn Bevolkingsonderzoek niet volledig </AlertTitle>
				Dat komt omdat u gebruik maakt van een oudere webbrowser. Een webbrowser is het programma dat u gebruikt voor de toegang tot internet.
				Oudere browsers zijn minder veilig en werken niet precies hetzelfde als nieuwere versies.
				Wij raden u aan om uw browser te updaten. U kunt bijvoorbeeld gebruik maken van Google Chrome, Mozilla Firefox of Microsoft Edge.
			</Alert>
		</Collapse>
	)
}

export default UnsupportedBrowserBanner
