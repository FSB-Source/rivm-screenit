
package nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.verslagen;

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

import java.util.Date;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.berichten.cda.MeldingOngeldigCdaBericht;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ClientService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.basic.MultiLineLabel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class BerichtInzienPanel extends GenericPanel<MeldingOngeldigCdaBericht>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientService clientService;

	public BerichtInzienPanel(String id, IModel<MeldingOngeldigCdaBericht> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		BootstrapDialog confirmDialog = new BootstrapDialog("confirmDialog");
		add(confirmDialog);
		add(DateLabel.forDatePattern("ontvangenCdaBericht.ontvangen", "dd-MM-yyyy HH:mm:ss"));
		add(new Label("ontvangenCdaBericht.berichtType"));
		add(new Label("melding"));
		MeldingOngeldigCdaBericht cdaMelding = model.getObject();
		OntvangenCdaBericht ontvangenCdaBericht = cdaMelding.getOntvangenCdaBericht();
		add(new MultiLineLabel("content", CdaTransformerHelper.cdaToHtml(ontvangenCdaBericht)).setEscapeModelStrings(false));

		add(new Label("uitvoerendeOrganisatie.naam"));
		add(new Label("uitvoerendeOrganisatie.organisatieType").setVisible(cdaMelding.getUitvoerendeOrganisatie() != null));
		add(new Label("uitvoerder.naamVolledig").setVisible(cdaMelding.getUitvoerder() != null));
		add(new Label("bsn"));
		Client client = clientService.getClientByBsn(cdaMelding.getBsn());
		Date geboortedatum = null;
		if (client != null)
		{
			geboortedatum = client.getPersoon().getGeboortedatum();
		}
		add(DateLabel.forDatePattern("geboortedatum", Model.of(geboortedatum), "dd-MM-yyyy"));

		final boolean magVerwijderen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_VERWERKEN_ONGELIDGE_BERICHTEN, Actie.VERWIJDEREN);
		add(new ConfirmingIndicatingAjaxLink<MeldingOngeldigCdaBericht>("verwijderen", getModel(), confirmDialog, "ongeldigBerichtVerwijderen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				BerichtInzienPanel.this.verwijderen(getModel(), target);
			}

		}.setVisible(magVerwijderen));

		final boolean magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_VERWERKEN_ONGELIDGE_BERICHTEN, Actie.AANPASSEN)
			&& Boolean.TRUE.equals(getModelObject().getHerstelbaar());

		add(new IndicatingAjaxLink<MeldingOngeldigCdaBericht>("opnieuwAanbieden")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				BerichtInzienPanel.this.opnieuwAanbieden(BerichtInzienPanel.this.getModel(), target);

			}

		}.setVisible(magAanpassen));
	}

	protected abstract void opnieuwAanbieden(IModel<MeldingOngeldigCdaBericht> model, AjaxRequestTarget target);

	protected abstract void verwijderen(IModel<MeldingOngeldigCdaBericht> model, AjaxRequestTarget target);
}
