package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;

import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.util.DateUtil;

import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public abstract class AbstractMammaRondePanel extends AbstractBEAccordionPanel<MammaBeoordeling>
{
	public AbstractMammaRondePanel(String id, IModel<MammaBeoordeling> model, IModel<String> title)
	{
		super(id, model, title, 12);
	}

	public AbstractMammaRondePanel(String id, IModel<MammaBeoordeling> model)
	{
		this(id, model, (Integer) null);
	}

	public AbstractMammaRondePanel(String id, IModel<MammaBeoordeling> model, Integer jaarLaatsteVerwijzing)
	{
		super(id, model, 12);
		MammaBeoordeling beoordeling = getModelObject();
		MammaOnderzoek onderzoek = beoordeling.getOnderzoek();
		LocalDate localDate = DateUtil.toLocalDate(onderzoek.getCreatieDatum());
		String title = "" + localDate.getYear();
		if (onderzoek.getBeoordelingen().size() > 1 && onderzoek.getLaatsteBeoordeling().equals(beoordeling))
		{
			title += " (HERBEOORDELING)";
		}
		if (beoordeling.getStatus() == MammaBeoordelingStatus.UITSLAG_ONGUNSTIG || jaarLaatsteVerwijzing != null)
		{
			super.setNamePostfixCssClass("fa fa-exclamation-triangle");
		}
		if (jaarLaatsteVerwijzing != null)
		{
			title += String.format(" laatst verwezen in %s", jaarLaatsteVerwijzing);
		}
		super.setTitle(Model.of(title));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		renderPanelComponents();
	}

	protected abstract void renderPanelComponents();
}
