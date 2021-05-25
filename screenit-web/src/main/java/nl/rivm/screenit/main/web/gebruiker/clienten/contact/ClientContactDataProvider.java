
package nl.rivm.screenit.main.web.gebruiker.clienten.contact;

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
import java.util.List;

import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientContactDataProvider extends SortableDataProvider<ClientContact, String>
{

	private static final long serialVersionUID = 1L;

	private final IModel<Client> zoekModel;

	private final List<Bevolkingsonderzoek> bevolkingsonderzoeken;

	@SpringBean
	private DossierService dossierService;

	public ClientContactDataProvider(IModel<Client> clientModel, List<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		this.zoekModel = clientModel;
		this.bevolkingsonderzoeken = bevolkingsonderzoeken;
		setSort("datum", SortOrder.DESCENDING);
		Injector.get().inject(this);
	}

	@Override
	public Iterator<? extends ClientContact> iterator(long first, long count)
	{
		return dossierService.getClientContacten(ModelUtil.nullSafeGet(zoekModel), bevolkingsonderzoeken, first, count, getSort().getProperty(), getSort().isAscending());
	}

	@Override
	public long size()
	{
		return dossierService.countClientContacten(ModelUtil.nullSafeGet(zoekModel), bevolkingsonderzoeken);
	}

	@Override
	public IModel<ClientContact> model(ClientContact object)
	{
		return ModelUtil.sModel(object);
	}

}
