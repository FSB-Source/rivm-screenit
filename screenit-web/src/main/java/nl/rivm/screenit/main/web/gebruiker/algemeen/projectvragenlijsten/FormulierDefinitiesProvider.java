package nl.rivm.screenit.main.web.gebruiker.algemeen.projectvragenlijsten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.VragenlijstService;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.vragenlijsten.Vragenlijst;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.HibernateDataProvider;

import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class FormulierDefinitiesProvider extends HibernateDataProvider<ScreenitFormulierInstantie>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private VragenlijstService vragelijstService;

	private IModel<? extends Vragenlijst> vragenlijst;

	public FormulierDefinitiesProvider(IModel<ScreenitFormulierInstantie> searchObjectModel, String defaultSortProperty, IModel<? extends Vragenlijst> vragenlijst)
	{
		super(searchObjectModel, defaultSortProperty);
		this.vragenlijst = vragenlijst;
	}

	@Override
	public Iterator<ScreenitFormulierInstantie> iterator(long first, long count)
	{
		return vragelijstService.getAlleFormulierenMetResultatenEnHuidige(vragenlijst.getObject());
	}

	@Override
	public long size()
	{
		return vragelijstService.countAlleFormulierenMetResultatenEnHuidige(vragenlijst.getObject());
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(vragenlijst);
	}
}
