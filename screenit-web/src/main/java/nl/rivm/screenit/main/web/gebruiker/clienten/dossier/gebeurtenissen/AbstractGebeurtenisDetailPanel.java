package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public class AbstractGebeurtenisDetailPanel extends GenericPanel<ScreeningRondeGebeurtenis>
{

	public AbstractGebeurtenisDetailPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
	}

	protected void addButton(String id, GebeurtenisPopupBasePanel parent)
	{
		Button button = new Button(id);
		parent.add(button);
		button.setVisible(false);
		button.add(new Label("label", getString("label.verwijderen")));
	}

	protected void addExtraButton(String id, GebeurtenisPopupBasePanel parent)
	{
		Button button = new Button(id);
		button.setVisible(false);
		button.add(new Label("label", "onzichtbaar"));
		parent.add(button);
	}

	protected void addDocumentVervangenButton(String id, GebeurtenisPopupBasePanel parent)
	{
		Button button = new Button(id);
		button.setVisible(false);
		parent.add(button);
	}

	protected void addDocumentDownloadenButton(String id, GebeurtenisPopupBasePanel parent)
	{
		Button button = new Button(id);
		button.setVisible(false);
		parent.add(button);
	}
}
