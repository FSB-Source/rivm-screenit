/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import BasePage from "../BasePage"
import styles from "./LabformulierenAanvragenPage.module.scss"
import properties from "./LabformulierenAanvragenPage.json"
import {getString} from "../../util/TekstPropertyUtil"
import BaseTabelComponent from "../../components/tabel/BaseTabelComponent"
import {LabformulierAanvragenOverzichtDto} from "../../state/datatypes/dto/LabformulierAanvragenOverzichtDto"
import {getAanvraagStatusText} from "../../state/datatypes/dto/AanvraagDto"
import classNames from "classnames"
import React from "react"
import {formatDateTime} from "../../util/DateUtil"
import {Col, Row} from "react-bootstrap"
import TabelPagineringComponent from "../../components/tabel/TabelPagineringComponent"

const LabformulierenAanvragenPage = () => {
	return <BasePage title={getString(properties.title)}>
		<div className={styles.style}>
			<div className={"d-block mx-2 mb-3"}>{getString(properties.algemeneMeldingen.geenLabformulieren)}
				<a href="https:
				   target="_blank" rel="noreferrer">{getString(properties.algemeneMeldingen.onzeWebsite)}</a>
				<div className={"my-2"}>{getString(properties.algemeneMeldingen.telefonischContact)}</div>
				<div className={styles.table}>
					<div className={styles.tr}>
						<span>Noord</span>
						<span><a href="tel:0505208888">050 – 520 88 88</a></span>
					</div>
					<div className={styles.tr}>
						<span>Oost</span>
						<span><a href="tel:0881186330">088 – 118 63 30</a></span>
					</div>
					<div className={styles.tr}>
						<span>Zuid</span>
						<span><a href="tel:0880001322">088 – 000 13 22</a></span>
					</div>
					<div className={styles.tr}>
						<span>Midden West</span>
						<span><a href="tel:0882669020">088 – 266 90 20</a></span>
					</div>
					<div className={styles.tr}>
						<span>Zuid West</span>
						<span><a href="tel:0882482000 ">088 – 248 20 00</a></span>
					</div>
				</div>
			</div>
			<BaseTabelComponent<LabformulierAanvragenOverzichtDto> resultsPerPage={10} url={"/aanvragen/huisarts"}>
				{(props) => (
					<>
						<h3 className={styles.historyOverzicht}>{getString(properties.table.title)}</h3>
						<div className={classNames(styles.tableContainer, "mb-3")}>
							<table>
								<thead>
								<tr>
									<th onClick={() => props.setSortProperty("aanvraagDatum")}>{getString(properties.table.header.aangevraagdOp)}</th>
									<th onClick={() => props.setSortProperty("aantal")}>{getString(properties.table.header.aantal)}</th>
									<th onClick={() => props.setSortProperty("status")}>{getString(properties.table.header.status)}</th>
									<th onClick={() => props.setSortProperty("statusDatum")}>{getString(properties.table.header.statusDatum)}</th>
									<th onClick={() => props.setSortProperty("aangevraagdDoor")}>{getString(properties.table.header.aangevraagdDoor)}</th>
									<th onClick={() => props.setSortProperty("locatie.naam")}>{getString(properties.table.header.locatie)}</th>
								</tr>
								</thead>
								<tbody>
								{props.results?.aanvragen.map(aanvraag => {
									return <tr key={aanvraag.huisartsportaalId}>
										<td>{aanvraag.aanvraagDatum && formatDateTime(new Date(aanvraag.aanvraagDatum))}</td>
										<td>{aanvraag.aantal}</td>
										<td>{aanvraag.status && getAanvraagStatusText[aanvraag.status]}</td>
										<td>{aanvraag.statusDatum && formatDateTime(new Date(aanvraag.statusDatum))}</td>
										<td>{aanvraag.aangevraagdDoor}</td>
										<td>{aanvraag.locatie.naam}</td>
									</tr>
								})}
								</tbody>
							</table>
						</div>
						<Row>
							<Col md={6}>
								<span>{getString(properties.table.totaal)} <b>{props.results?.aantalAanvragen ?? 0}</b></span>
							</Col>
							<Col md={6}>
								<Row>
									<TabelPagineringComponent
										pageCount={Math.ceil(Math.max(1, props.results?.aantalAanvragen ?? 0) / 10)} page={props.page}
										setPage={props.setPage}
									/>
								</Row>
							</Col>
						</Row>
					</>
				)}
			</BaseTabelComponent>
		</div>
	</BasePage>
}

export default LabformulierenAanvragenPage
