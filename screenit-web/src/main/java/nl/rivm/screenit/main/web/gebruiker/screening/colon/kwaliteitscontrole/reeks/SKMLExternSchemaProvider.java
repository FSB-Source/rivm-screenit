package nl.rivm.screenit.main.web.gebruiker.screening.colon.kwaliteitscontrole.reeks;

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

import nl.rivm.screenit.main.service.SKMLExternSchemaService;
import nl.rivm.screenit.model.colon.SKMLExternSchema;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class SKMLExternSchemaProvider extends SortableDataProvider<SKMLExternSchema, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private SKMLExternSchemaService schemaService;

	private IModel<SKMLExternSchema> zoekModel;

	public SKMLExternSchemaProvider(String sortProperty, SortOrder sortOrder, IModel<SKMLExternSchema> schemaModel)
	{
		this.zoekModel = schemaModel;
		setSort(sortProperty, sortOrder);
		Injector.get().inject(this);
	}

	@Override
	public Iterator<? extends SKMLExternSchema> iterator(long first, long count)
	{
		return schemaService.zoekSchemas(ModelUtil.nullSafeGet(zoekModel), getSort().getProperty(), getSort().isAscending(), (int) first, (int) count);
	}

	@Override
	public long size()
	{
		return schemaService.telSchemas(ModelUtil.nullSafeGet(zoekModel));
	}

	@Override
	public IModel<SKMLExternSchema> model(SKMLExternSchema object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(zoekModel);
	}

}
