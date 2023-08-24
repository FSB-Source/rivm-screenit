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
export enum AfspraakBevestigingsWizardStap {
	AFSPRAAK_MAKEN_MISLUKT, AFSPRAAK_MAKEN, AFSPRAAK_BEVESTIGINGSMAIL, AFSPRAAK_SMS_HERINNERING, AFSPRAAK_OVERZICHT, HUISARTS_DOORGEVEN
}

export function getAfspraakBevestingWizardStappen(magHerinnering: boolean): AfspraakBevestigingsWizardStap[] {
	return magHerinnering ? [AfspraakBevestigingsWizardStap.AFSPRAAK_MAKEN, AfspraakBevestigingsWizardStap.AFSPRAAK_BEVESTIGINGSMAIL, AfspraakBevestigingsWizardStap.AFSPRAAK_SMS_HERINNERING, AfspraakBevestigingsWizardStap.AFSPRAAK_OVERZICHT, AfspraakBevestigingsWizardStap.HUISARTS_DOORGEVEN]
		: [AfspraakBevestigingsWizardStap.AFSPRAAK_MAKEN, AfspraakBevestigingsWizardStap.AFSPRAAK_BEVESTIGINGSMAIL, AfspraakBevestigingsWizardStap.AFSPRAAK_OVERZICHT, AfspraakBevestigingsWizardStap.HUISARTS_DOORGEVEN]
}
