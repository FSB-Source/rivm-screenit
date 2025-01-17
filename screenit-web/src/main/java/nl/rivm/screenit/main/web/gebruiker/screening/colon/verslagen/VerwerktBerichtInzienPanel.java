
package nl.rivm.screenit.main.web.gebruiker.screening.colon.verslagen;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.verslagen.CdaTransformerHelper;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.basic.MultiLineLabel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;

public abstract class VerwerktBerichtInzienPanel extends GenericPanel<OntvangenCdaBericht>
{

	private static final long serialVersionUID = 1L;

	public VerwerktBerichtInzienPanel(String id, IModel<OntvangenCdaBericht> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(DateLabel.forDatePattern("ontvangen", "dd-MM-yyyy HH:mm:ss"));
		add(new Label("berichtType"));
		add(new MultiLineLabel("content", CdaTransformerHelper.cdaToHtml(model.getObject())).setEscapeModelStrings(false));

		final boolean magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_VERSLAGEN, Actie.AANPASSEN);

		add(new IndicatingAjaxLink<OntvangenCdaBericht>("opnieuwAanbieden")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				VerwerktBerichtInzienPanel.this.opnieuwAanbieden(VerwerktBerichtInzienPanel.this.getModel(), target);

			}

		}.setVisible(magAanpassen));
	}

	protected abstract void opnieuwAanbieden(IModel<OntvangenCdaBericht> model, AjaxRequestTarget target);

}
