package nl.rivm.screenit.model.project;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

public enum ProjectInactiefReden
{
	AFMELDING("Afmelding"),

	BEZWAAR("Bezwaar"),

	OVERLEDEN("Overleden"),

	VERTROKKEN_UIT_NEDERLAND("Vertrokken uit Nederland"),

	VIA_INFOLIJN("Cliënt geïnactiveerd via infolijn"),

	VIA_CLIENTPORTAAL("Cliënt geïnactiveerd via het clientportaal"),

	INACTIVATIE_DOOR_UPLOAD("Cliënt geïnactiveerd door upload"),

	INACTIVATIE_DOOR_EXCLUSIE_OPENRONDE("Cliënt geïnactiveerd door exclusie open ronde"),

	VERWIJDERING_VAN_DOSSIER("Cliënt geïnactiveerd vanwege verwijdering van dossier"),

	VALT_UIT_2DE_BUIS_PROJECT("Cliënt geïnactiveerd door afwijkingen in het vergelijkend onderzoek project"),

	INACTIVATIE_VERGELIJKEND_ONDERZOEK("Cliënt geïnactiveerd door afwijkingen in het vergelijkend onderzoek project");

	public final String naam;

	private ProjectInactiefReden(String naam)
	{
		this.naam = naam;
	}

}
