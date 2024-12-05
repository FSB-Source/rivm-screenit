package nl.rivm.screenit.factory.algemeen;

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

import java.util.Date;

import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.repository.BaseJpaRepository;

public interface BriefFactory
{
	<B extends ClientBrief<?, ?, ?>> BaseJpaRepository<B> getBriefTypeRepository(Class<B> briefClass);

	BezwaarBrief maakBezwaarBrief(Client client, BriefType type, Date date);

	BezwaarBrief maakBezwaarBrief(Client client, BriefType type, Date date, boolean vragenOmHandtekening);

	AlgemeneBrief maakAlgemeneBrief(Client client, BriefType type);

	<B extends ClientBrief<?, A, ?>, A extends Afmelding<?, ?, B>> B maakBvoBrief(A afmelding, BriefType type, Date creatieMoment, boolean vervangendeProjectBrief);

	<B extends ClientBrief<?, A, ?>, A extends Afmelding<?, ?, B>> B maakBvoBrief(A afmelding, BriefType type, Date creatieMoment);

	<B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type);

	<B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, boolean gegenereerd);

	<B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, Date creatieMoment);

	<B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, Date creatieMoment, boolean gegenereerd,
		boolean vervangendeProjectBrief);

	CervixRegioBrief maakRegioBrief(ScreeningOrganisatie so, BriefType type, Date date, CervixHuisarts arts);

	ProjectBrief maakProjectBrief(ProjectClient pClient, ProjectBriefActie actie, ProjectBrief origineleBrief);
}
