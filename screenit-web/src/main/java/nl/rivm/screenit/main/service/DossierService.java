package nl.rivm.screenit.main.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.model.DossierGebeurtenis;
import nl.rivm.screenit.main.model.GebeurtenisBron;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenissen;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.hibernate.envers.query.criteria.AuditCriterion;

public interface DossierService
{

	List<ScreeningRondeGebeurtenissen> getScreeningRondeGebeurtenissen(Client client, ClientDossierFilter clientDossierFilter);

	Iterator<ClientContact> getClientContacten(Client client, List<Bevolkingsonderzoek> bevolkingsonderzoeken, long first, long count, String sortProperty, boolean ascending);

	int countClientContacten(Client client, List<Bevolkingsonderzoek> bevolkingsonderzoeken);

	List<DossierGebeurtenis> getMammaDossierGebeurtenissen(Client client);

	List<ScreeningRondeGebeurtenis> getProjectGebeurtenissen(ProjectClient pClient);

	List<ClientGebeurtenis> getClientColonGebeurtenissen(Client client);

	List<ClientGebeurtenis> getClientCervixGebeurtenissen(Client client);

	List<ClientGebeurtenis> getClientMammaGebeurtenissen(Client client);

	GebeurtenisBron bepaalGebeurtenisBron(HibernateObject entity);

	GebeurtenisBron bepaalGebeurtenisBron(HibernateObject entity, AuditCriterion extraCriteria);

	List<DossierGebeurtenis> getColonDossierGebeurtenissen(Client client);

	List<DossierGebeurtenis> getCervixDossierGebeurtenissen(Client client);

}
