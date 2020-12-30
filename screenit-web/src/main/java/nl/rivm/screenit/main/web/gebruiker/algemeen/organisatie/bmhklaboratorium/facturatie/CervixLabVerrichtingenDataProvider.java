package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.bmhklaboratorium.facturatie;

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

import nl.rivm.screenit.dto.cervix.facturatie.CervixVerrichtingenZoekObject;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.service.cervix.CervixVerrichtingService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class CervixLabVerrichtingenDataProvider extends SortableDataProvider<CervixBoekRegel, String>
{

	private IModel<CervixVerrichtingenZoekObject> cervixVerrichtingenCriteria;

	private IModel<BMHKLaboratorium> geselecteerdeOrganisatie;

	private IModel<ScreeningOrganisatie> screeningOrganisatieModel;

	@SpringBean
	private CervixVerrichtingService cervixVerrichtingService;

	public CervixLabVerrichtingenDataProvider(IModel<CervixVerrichtingenZoekObject> cervixVerrichtingenCriteria, IModel<ScreeningOrganisatie> screeningOrganisatieModel,
		IModel<BMHKLaboratorium> geselecteerdeOrganisatie)
	{
		Injector.get().inject(this);
		setSort("verrichting.verrichtingsDatum", SortOrder.DESCENDING);
		this.cervixVerrichtingenCriteria = cervixVerrichtingenCriteria;
		this.geselecteerdeOrganisatie = geselecteerdeOrganisatie;
		this.screeningOrganisatieModel = screeningOrganisatieModel;
	}

	@Override
	public Iterator<? extends CervixBoekRegel> iterator(long first, long count)
	{
		return cervixVerrichtingService.getLabVerrichtingen(cervixVerrichtingenCriteria.getObject(), screeningOrganisatieModel.getObject(), geselecteerdeOrganisatie.getObject(),
			new SortState<>(getSort().getProperty(), getSort().isAscending()), first, count)
			.iterator();
	}

	@Override
	public long size()
	{
		return cervixVerrichtingService
			.countLabVerrichtingen(cervixVerrichtingenCriteria.getObject(), screeningOrganisatieModel.getObject(), geselecteerdeOrganisatie.getObject());
	}

	@Override
	public IModel<CervixBoekRegel> model(CervixBoekRegel object)
	{
		return ModelUtil.sModel(object);
	}

	public BMHKLaboratorium getGeselecteerdeOrganisatie()
	{
		return ModelUtil.nullSafeGet(geselecteerdeOrganisatie);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(cervixVerrichtingenCriteria);
		ModelUtil.nullSafeDetach(geselecteerdeOrganisatie);
		ModelUtil.nullSafeDetach(screeningOrganisatieModel);
	}
}
