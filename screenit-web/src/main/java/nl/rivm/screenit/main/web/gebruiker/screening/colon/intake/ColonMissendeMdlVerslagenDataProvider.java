
package nl.rivm.screenit.main.web.gebruiker.screening.colon.intake;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Iterator;

import nl.rivm.screenit.main.service.colon.ColonIntakeAfspraakService;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ColonMissendeMdlVerslagenDataProvider extends SortableDataProvider<ColonIntakeAfspraak, String>
{

	private final IModel<WerklijstIntakeFilter> zoekModel;

	@SpringBean
	private ColonIntakeAfspraakService colonAfspraakService;

	@SpringBean
	private AfspraakService afspraakService;

	private int aantalPerPagina;

	private final IModel<ColoscopieCentrum> intakeLocatie;

	public ColonMissendeMdlVerslagenDataProvider(IModel<WerklijstIntakeFilter> zoekModel, ColoscopieCentrum intakeLocatie, int aantalPerPagina)
	{
		this.zoekModel = zoekModel;
		this.intakeLocatie = ModelUtil.sModel(intakeLocatie);
		this.aantalPerPagina = aantalPerPagina;
		setSort("conclusie.datumColoscopie", SortOrder.ASCENDING);
		Injector.get().inject(this);
	}

	@Override
	public Iterator<? extends ColonIntakeAfspraak> iterator(long first, long count)
	{
		return colonAfspraakService.getAfsprakenZonderVerslag(ModelUtil.nullSafeGet(zoekModel), ModelUtil.nullSafeGet(intakeLocatie), first,
			count > aantalPerPagina ? count : aantalPerPagina,
			getSort().getProperty(), getSort().isAscending()).iterator();
	}

	@Override
	public long size()
	{
		return colonAfspraakService.getAantalAfsprakenZonderVerslag(ModelUtil.nullSafeGet(zoekModel), ModelUtil.nullSafeGet(intakeLocatie));
	}

	@Override
	public IModel<ColonIntakeAfspraak> model(ColonIntakeAfspraak object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(zoekModel);
		ModelUtil.nullSafeDetach(intakeLocatie);
	}
}
