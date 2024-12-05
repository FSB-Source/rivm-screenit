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
import {Checkbox} from "@mui/material"
import React from "react"
import {Bezwaar} from "../../datatypes/Bezwaar"
import BezwaarInformatieBlok from "./BezwaarInformatieBlok"
import styles from "./BezwaarBlok.module.scss"

export type BezwaarBlokProps = {
	bezwaar: Bezwaar
	onChange: (bezwaar: Bezwaar) => void;
	abstract: string;
	meer: string;
	standalone: boolean;
	titel: string;
}
const BezwaarBlok = (props: BezwaarBlokProps) => {
	return (<div key={props.bezwaar.type} className={styles.bezwaarBlok}>
		<div className={styles.checkBox}>
			<Checkbox name={props.bezwaar.type}
					  id={props.bezwaar.type}
					  defaultChecked={props.bezwaar.active}
					  onChange={(event) => {
						  props.onChange({
							  type: props.bezwaar.type,
							  active: event.target.checked,
							  bevolkingsonderzoek: props.bezwaar.bevolkingsonderzoek,
						  })
					  }}/>
			<label className={styles.label}
				   htmlFor={props.bezwaar.type}>{props.titel}</label>
		</div>
		<BezwaarInformatieBlok abstract={props.abstract} meer={props.meer} standalone={props.standalone}/>
	</div>)
}

export default BezwaarBlok
