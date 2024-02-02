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
import {useAppSelector, useAppThunkDispatch} from "../../../index"
import properties from "./LoginPage.json"
import {useNavigate} from "react-router"
import {CredentialsDto} from "../../../state/datatypes/dto/CredentialsDto"
import {getString} from "../../../util/TekstPropertyUtil"
import BaseAuthenticationPage from "../BaseAuthenticationPage"
import {loadingThunkAction} from "../../../api/LoadingThunkAction"
import {authenticate} from "../../../api/AanmeldenThunkAction"
import {AuthenticationScope} from "../../../state/datatypes/enums/AuthenticationScope"
import FormTextField from "../../../components/form/text/FormTextField"
import FormPasswordTextField from "../../../components/form/text/FormPasswordTextField"
import {Col, Row} from "react-bootstrap"
import styles from "./LoginPage.module.scss"
import React, {useEffect} from "react"

export const LoginPage = () => {
	const dispatch = useAppThunkDispatch()
	const navigate = useNavigate()
	const oauth = useAppSelector(state => state.oauth)

	useEffect(() => {
		if (oauth) {
			navigate("/")
		}
	}, [oauth, navigate])

	return (
		<BaseAuthenticationPage<CredentialsDto>
			title={getString(properties.title)}
			description={getString(properties.description)}
			submitText={getString(properties.form.buttons.submit)}
			initialValues={{
				gebruikersnaam: "",
				wachtwoord: "",
			}}
			onSubmit={(credentials => {
				dispatch(loadingThunkAction(authenticate(credentials.gebruikersnaam, credentials.wachtwoord, AuthenticationScope.LOGIN))).then(() => {
					navigate("/")
				})
			})}>
			{() => (<div className={styles.style}>
				<FormTextField className={"row my-3"} property={"gebruikersnaam"} label={getString(properties.form.labels.username)}/>
				<FormPasswordTextField className={"row mt-3 mb-1"} property={"wachtwoord"} label={getString(properties.form.labels.password)}/>
				<Row className={"row"}>
					<Col md={4}/>
					<Col md={8}><a href={"/wachtwoordvergeten"}>Wachtwoord opnieuw instellen</a></Col>
				</Row>
			</div>)}
		</BaseAuthenticationPage>
	)
}
