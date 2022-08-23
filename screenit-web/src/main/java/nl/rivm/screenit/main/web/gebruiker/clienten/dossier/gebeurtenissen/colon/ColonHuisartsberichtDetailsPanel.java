package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.colon;

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

import nl.rivm.screenit.main.model.BaseHuisartsModel;
import nl.rivm.screenit.main.model.EnovationHuisartsModel;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.gebruiker.algemeen.huisarts.HuisartsInfoPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.huisarts.HuisartsZoekenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.ClientDossierPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.GebeurtenisPopupBasePanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonHuisartsBerichtStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.colon.ColonHuisartsBerichtService;
import nl.rivm.screenit.util.HuisartsBerichtenUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_CLIENT_SR_HUISARTSBERICHT_DETAILS,
	bevolkingsonderzoekScopes = Bevolkingsonderzoek.COLON)
public class ColonHuisartsberichtDetailsPanel extends AbstractGebeurtenisDetailPanel
{

	@SpringBean
	private ColonHuisartsBerichtService huisartsBerichtService;

	private IndicatingAjaxLink wijzigHuisartsButton;

	private WebMarkupContainer huisartsBerichtContainer;

	private WebMarkupContainer huisartsZoekenContainer;

	private IModel<EnovationHuisarts> selectedHuisarts;

	private BootstrapDialog dialog;

	public ColonHuisartsberichtDetailsPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		onHuisartsGekozen(getHuisarts());
		huisartsBerichtContainer = maakHuisartsBerichtContainer(true);
		add(huisartsBerichtContainer);

		huisartsZoekenContainer = maakHuisartsZoekenPanel();
		add(huisartsZoekenContainer);

		dialog = new BootstrapDialog("dialog");
		add(dialog);

	}

	@Override
	protected void addButton(String id, GebeurtenisPopupBasePanel parent)
	{
		ConfirmingIndicatingAjaxLink verstuurButton = new ConfirmingIndicatingAjaxLink<Void>(id, dialog, "question.opnieuwversturen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				verstuurHuisartsBericht(getHuisartsBericht());
				setResponsePageClientDossier();
			}

			@Override
			protected IModel<String> getContentStringModel()
			{
				String formatted = String.format(super.getContentStringModel().getObject(), getSelectedHuisarts().getPraktijknaam(), getHuisarts().getPraktijknaam());
				return Model.of(formatted);
			}

			@Override
			protected boolean skipConfirmation()
			{
				return getHuisarts().equals(getSelectedHuisarts());
			}
		};

		verstuurButton.add(new Label("label", getStandardButtonText()));
		verstuurButton.setVisible(heeftRechtOpnieuwVersturen());
		verstuurButton.setOutputMarkupId(true);
		verstuurButton.setOutputMarkupPlaceholderTag(true);
		parent.add(verstuurButton);
	}

	@Override
	protected void addExtraButton(String id, GebeurtenisPopupBasePanel parent)
	{
		wijzigHuisartsButton = new IndicatingAjaxLink<Void>(id)
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				refreshContainer(target, false);
				refreshZoekPanel(target, true);
				refreshWijzigenButton(target, false);
			}
		};

		wijzigHuisartsButton.setOutputMarkupId(true);
		wijzigHuisartsButton.setOutputMarkupPlaceholderTag(true);
		wijzigHuisartsButton.setVisible(heeftRechtOpnieuwVersturen());
		wijzigHuisartsButton.add(new Label("label", getHuisartsWijzigTekst()));
		parent.add(wijzigHuisartsButton);
	}

	private void refreshContainer(AjaxRequestTarget target, boolean visible)
	{
		WebMarkupContainer container = maakHuisartsBerichtContainer(visible);
		huisartsBerichtContainer.replaceWith(container);
		huisartsBerichtContainer = container;
		target.add(huisartsBerichtContainer);
	}

	private WebMarkupContainer maakHuisartsBerichtContainer(boolean visible)
	{
		WebMarkupContainer container = new WebMarkupContainer("huisartsBerichtContainer");
		container.setVisible(visible);
		container.setOutputMarkupId(true);
		container.setOutputMarkupPlaceholderTag(true);
		container.add(new Label("colonHuisartsBericht.berichtType.naam"));
		container.add(new Label("colonHuisartsBericht.aanmaakDatum"));
		container.add(new EnumLabel<ColonHuisartsBerichtStatus>("colonHuisartsBericht.status"));

		EnovationHuisarts huisarts = getSelectedHuisarts();
		BaseHuisartsModel<?> huisartsModel = new EnovationHuisartsModel(huisarts);
		HuisartsInfoPanel result = new HuisartsInfoPanel("huisartsInfo", huisartsModel);
		result.setOutputMarkupId(true);
		result.setOutputMarkupPlaceholderTag(true);
		result.setVisible(huisarts != null);
		container.add(result);
		return container;
	}

	private void refreshZoekPanel(AjaxRequestTarget target, boolean visible)
	{
		WebMarkupContainer nieuw = maakHuisartsZoekenPanel();
		nieuw.setVisible(visible);
		huisartsZoekenContainer.replaceWith(nieuw);
		huisartsZoekenContainer = nieuw;
		target.add(huisartsZoekenContainer);
	}

	private void refreshWijzigenButton(AjaxRequestTarget target, boolean visible)
	{
		wijzigHuisartsButton.setVisible(visible);
		target.add(wijzigHuisartsButton);

	}

	private HuisartsZoekenPanel maakHuisartsZoekenPanel()
	{
		HuisartsZoekenPanel result = new HuisartsZoekenPanel("huisartsZoeken", false)
		{
			@Override
			protected void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts)
			{
				ColonHuisartsberichtDetailsPanel.this.onHuisartsGekozen(huisarts);
				refreshContainer(target, true);
				refreshZoekPanel(target, false);
				refreshWijzigenButton(target, false);
			}

		};
		result.setOutputMarkupId(true);
		result.setOutputMarkupPlaceholderTag(true);
		result.setVisible(selectedHuisarts == null);
		return result;
	}

	private String getStandardButtonText()
	{

		if (getHuisartsBericht().getStatus() == ColonHuisartsBerichtStatus.VERZENDEN_MISLUKT)
		{
			return getString("huisartsbericht.versturen");
		}
		else
		{
			return getString("huisartsbericht.opnieuw.versturen");
		}
	}

	private String getHuisartsWijzigTekst()
	{
		if (getHuisarts() == null)
		{
			return getString("huisartsbericht.huisarts.koppelen");
		}
		else
		{
			return getString("huisartsbericht.versturen.extra.huisarts");

		}
	}

	private void verstuurHuisartsBericht(ColonHuisartsBericht huidigBericht)
	{
		ColonHuisartsBericht verstuurdBericht = huisartsBerichtService.verstuurHuisartsBericht(huidigBericht, getSelectedHuisarts());
		ColonHuisartsBerichtStatus status = verstuurdBericht.getStatus();
		switch (status)
		{
		case VERZENDEN_GELUKT:
			ScreenitSession.get().info(verstuurdBericht.isEenOpnieuwVerzondenBericht() ? getString("huisartsbericht.opnieuw.verstuurd") : getString("huisartsbericht.verstuurd"));
			break;
		case VERZENDEN_MISLUKT:
			ScreenitSession.get()
				.info(verstuurdBericht.isEenOpnieuwVerzondenBericht() ? getString("huisartsbericht.versturen.mislukt") : getString("huisartsbericht.opnieuw.versturen.mislukt"));
			break;
		default:
			throw new IllegalStateException();
		}
	}

	private boolean heeftRechtOpnieuwVersturen()
	{
		boolean laatstVerstuurdeHuisartsbericht = HuisartsBerichtenUtil.isLaatstVerstuurdeHuisartsbericht(getHuisartsBericht());
		boolean permission = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_HUISARTSBERICHT_OPNIEUW_VERZENDEN, Actie.AANPASSEN,
			getHuisartsBericht().getClient());
		return laatstVerstuurdeHuisartsbericht && permission;
	}

	private void setResponsePageClientDossier()
	{
		setResponsePage(new ClientDossierPage(ModelUtil.sModel(getHuisartsBericht().getClient())));
	}

	private void onHuisartsGekozen(EnovationHuisarts huisarts)
	{
		selectedHuisarts = ModelUtil.sModel(huisarts);
	}

	private EnovationHuisarts getSelectedHuisarts()
	{
		return ModelUtil.nullSafeGet(selectedHuisarts);
	}

	private ColonHuisartsBericht getHuisartsBericht()
	{
		return getModelObject().getColonHuisartsBericht();
	}

	private EnovationHuisarts getHuisarts()
	{
		return getHuisartsBericht().getHuisarts();
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(selectedHuisarts);
	}
}
