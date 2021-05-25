package nl.rivm.screenit.main.web.gebruiker.screening.cervix.monster.popup;

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

import nl.rivm.screenit.model.cervix.CervixMonster;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class CervixMonsterBezwaarDialog<M extends CervixMonster> extends GenericPanel<CervixMonster>
{

	public CervixMonsterBezwaarDialog(String contentId, IModel<CervixMonster> model, boolean geenGebruikLichaamsMateriaal, boolean geenSignaleringAdvies)
	{
		super(contentId, model);
		WebMarkupContainer bezwaarlichaamsmateriaal = new WebMarkupContainer("bezwaarlichaamsmateriaal");
		WebMarkupContainer verwijsadvies = new WebMarkupContainer("verwijsadvies");
		bezwaarlichaamsmateriaal.setVisible(geenGebruikLichaamsMateriaal);
		verwijsadvies.setVisible(geenSignaleringAdvies);
		bezwaarlichaamsmateriaal.add(new Label("monsterId", getModelObject().getMonsterId()));
		add(bezwaarlichaamsmateriaal, verwijsadvies);
	}

	public abstract void close(AjaxRequestTarget target);
}
