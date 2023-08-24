package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.mamma;

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

import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.LoggingInzienPage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientInzienPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.logging.MammaHl7v24BerichtLogEvent;
import nl.rivm.screenit.service.ClientService;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_INZIEN_LOGGING,
	bevolkingsonderzoekScopes = Bevolkingsonderzoek.MAMMA)
public class MammaHl7v24BerichtPage extends AlgemeenPage implements IDetachable
{
	@SpringBean
	private ClientService clientService;

	private IModel<MammaHl7v24BerichtLogEvent> hl7BerichtLogEventModel;

	public MammaHl7v24BerichtPage(IModel<MammaHl7v24BerichtLogEvent> model)
	{
		hl7BerichtLogEventModel = CompoundPropertyModel.of(model);
		String hl7MessageStructure = hl7BerichtLogEventModel.getObject().getHl7MessageStructure();
		hl7MessageStructure = hl7MessageStructure.replace("\n", "<br>");
		hl7MessageStructure = hl7MessageStructure.replace("\t", "&#9;");
		Label messageStructure = new Label("messageStructure", hl7MessageStructure);
		messageStructure.setEscapeModelStrings(false);
		add(messageStructure);

		add(new Label("melding", hl7BerichtLogEventModel.getObject().getMelding()));

		clientdossierButtonToevoegenAanPagina();
	}

	private void clientdossierButtonToevoegenAanPagina()
	{
		AjaxLink<Void> clientDossierButton = new AjaxLink<Void>("directNaarClientDossier")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				var clientUitModel = hl7BerichtLogEventModel.getObject().getLogRegel().getClient();
				if (clientUitModel != null)
				{
					Client client = clientService
						.getClientByBsn(clientUitModel.getPersoon().getBsn());
					setResponsePage(new ClientInzienPage(new SimpleHibernateModel<>(client)));
				}
			}
		};
		Label directNaarClientDossierAlternatiefLabel = new Label("directNaarClientDossierAlternatief", "Geen client gekoppeld, bekijk HL7 bericht voor client BSN");
		if (hl7BerichtLogEventModel.getObject().getLogRegel().getClient() == null)
		{
			clientDossierButton.setVisible(false);
			directNaarClientDossierAlternatiefLabel.setVisible(true);
		}
		else
		{
			directNaarClientDossierAlternatiefLabel.setVisible(false);
			clientDossierButton.setVisible(true);
		}
		add(clientDossierButton);
		add(directNaarClientDossierAlternatiefLabel);
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveSubMenuClass()
	{
		return LoggingInzienPage.class;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(hl7BerichtLogEventModel);
	}
}
