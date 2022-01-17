package nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren;

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

import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class PreviewCellPanel extends GenericPanel<ScreenitFormulierInstantie>
{

	private static final long serialVersionUID = 1L;

	public PreviewCellPanel(String id, IModel<ScreenitFormulierInstantie> rowModel)
	{
		super(id, rowModel);
		add(new AjaxLink<Void>("onclick")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ScreenitFormulierInstantie formulierInstantie = PreviewCellPanel.this.getModelObject();
				if (formulierInstantie != null)
				{
					setResponsePage(new FormulierRenderPreviewPage(ModelUtil.sModel(formulierInstantie))
					{

						private static final long serialVersionUID = 1L;

						@Override
						protected Class<? extends GebruikerBasePage> getActiveSubMenuClass()
						{
							return PreviewCellPanel.this.getActiveSubMenuClass();
						}

						@Override
						protected GebruikerHoofdMenuItem getActieveMenuItem()
						{
							return GebruikerHoofdMenuItem.ALGEMEEN;
						}

						@Override
						protected void terug()
						{
							PreviewCellPanel.this.terug();
						}
					});
				}
			}

		});
	}

	protected abstract void terug();

	protected abstract Class<? extends GebruikerBasePage> getActiveSubMenuClass();
}
