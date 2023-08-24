/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import React from "react"
import {LocatieStatus} from "../../../state/datatypes/dto/LocatieDto"
import properties from "./LocatieStatusComponent.json"
import styles from "./LocatieStatusComponent.module.scss"
import {getString} from "../../../util/TekstPropertyUtil"
import classNames from "classnames"

export interface LocatieStatusComponentProps {
	status: LocatieStatus
}

export interface LocatieStatusDropdownChoice {
	value: LocatieStatus;
	label: string;
}

const LocatieStatusComponent = (props: LocatieStatusComponentProps) => {
	return <div
		className={classNames(styles.style, props.status === LocatieStatus.ACTIEF ? styles.statusActief : props.status === LocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD ? styles.statusNietGeverifieerd : styles.statusVerwijderd)}>
		{getString(props.status === LocatieStatus.ACTIEF ? properties.status.actief : props.status === LocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD ? properties.status.verifieren : properties.status.verwijderd)}
	</div>
}

export default LocatieStatusComponent
