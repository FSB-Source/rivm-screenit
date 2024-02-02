package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.MammaDagEnDagdeelFilter;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaKandidaatAfsprakenProvider extends SortableDataProvider<MammaKandidaatAfspraakDto, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private MammaBaseAfspraakService baseAfspraakService;

	private final IModel<Client> clientModel;

	private final IModel<MammaAfspraakWijzigenFilter> filterModel;

	private final MammaDagEnDagdeelFilter dagEnDagdeelFilter;

	private List<MammaKandidaatAfspraakDto> kandidaatAfspraken;

	private List<MammaKandidaatAfspraakDto> kandidaatAfsprakenCache = new ArrayList<>();

	private boolean lijstBehouden = false;

	public MammaKandidaatAfsprakenProvider(IModel<Client> clientModel, IModel<MammaAfspraakWijzigenFilter> filterModel, MammaDagEnDagdeelFilter dagEnDagdeelFilter)
	{
		Injector.get().inject(this);
		this.filterModel = filterModel;
		this.clientModel = clientModel;
		this.dagEnDagdeelFilter = dagEnDagdeelFilter;
	}

	@Override
	public Iterator<? extends MammaKandidaatAfspraakDto> iterator(long first, long count)
	{
		return kandidaatAfspraken.subList((int) first, (int) (first + count)).iterator();
	}

	@Override
	public long size()
	{
		if (!lijstBehouden)
		{
			kandidaatAfsprakenCache = baseAfspraakService.getKandidaatAfspraken(clientModel.getObject(), filterModel.getObject()).stream().distinct().sorted()
				.collect(Collectors.toList());
		}
		lijstBehouden = false;
		kandidaatAfspraken = baseAfspraakService.filterKandidaatAfsprakenOpDagEnDagdeel(kandidaatAfsprakenCache, dagEnDagdeelFilter);
		return kandidaatAfspraken.size();
	}

	@Override
	public IModel<MammaKandidaatAfspraakDto> model(MammaKandidaatAfspraakDto kandidaatAfspraakDto)
	{
		return Model.of(kandidaatAfspraakDto);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(filterModel);
		ModelUtil.nullSafeDetach(clientModel);
	}

	public void setLijstBehouden(boolean lijstBehouden)
	{
		this.lijstBehouden = lijstBehouden;
	}
}
