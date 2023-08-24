package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.cervix;

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

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.ClientDossierPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.GebeurtenisPopupBasePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren.CervixHuisartsLocatiePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren.CervixZoekHuisartsLocatiePanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixEdiService;
import nl.rivm.screenit.service.cervix.CervixVerrichtingFactory;
import nl.rivm.screenit.service.cervix.enums.CervixEdiVerstuurStatus;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_CLIENT_SR_HUISARTSBERICHT_DETAILS,
	bevolkingsonderzoekScopes = Bevolkingsonderzoek.CERVIX)
public class CervixHuisartsberichtDetailsPanel extends AbstractGebeurtenisDetailPanel
{
	private enum PanelState
	{
		Normaal,
		KoppelHuisarts,
		ExtraHuisarts
	}

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private CervixVerrichtingFactory cervixVerrichtingFactory;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private CervixEdiService ediService;

	private Label zorgmailLabel;

	private WebMarkupContainer huisartsLocatiePanel;

	private IndicatingAjaxLink standardButton;

	private IndicatingAjaxLink extraHuisartsButton;

	private IModel<CervixHuisartsLocatie> selectedHuisartsLocatie;

	private PanelState panelState;

	private BootstrapDialog dialog;

	public CervixHuisartsberichtDetailsPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		panelState = getInitialPanelState();

		add(zorgmailLabel = maakZorgmailLabel());
		add(new Label("huisartsBericht.berichtType.naam"));
		add(new Label("huisartsBericht.aanmaakDatum"));
		add(new EnumLabel<CervixHuisartsBerichtStatus>("huisartsBericht.status"));
		add(new Label("huisartsBericht.statusDatum"));

		huisartsLocatiePanel = maakHuisartsLocatieContainer();
		add(huisartsLocatiePanel);

		dialog = new BootstrapDialog("dialog");
		add(dialog);
	}

	private PanelState getInitialPanelState()
	{
		CervixHuisartsBericht huisartsBericht = getHuisartsBericht();
		CervixUitstrijkje uitstrijkje = huisartsBericht.getUitstrijkje(); 
		boolean magHuisartsKoppelen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_HUISARTS_KOPPELEN, Actie.AANPASSEN, huisartsBericht.getClient())
			&& uitstrijkje != null && uitstrijkje.getUitstrijkjeStatus() != CervixUitstrijkjeStatus.NIET_ONTVANGEN;
		boolean isHuisartsOnbekend = huisartsBericht.getStatus() == CervixHuisartsBerichtStatus.HUISARTS_ONBEKEND;
		return huisartsBericht.getHuisartsLocatie() == null && isHuisartsOnbekend && magHuisartsKoppelen ? PanelState.KoppelHuisarts : PanelState.Normaal;
	}

	private WebMarkupContainer maakHuisartsLocatieContainer()
	{
		switch (panelState)
		{
		case Normaal:
			IModel<CervixHuisartsLocatie> huisartsLocatie = new CompoundPropertyModel<>(new PropertyModel<>(getModel(), "huisartsBericht.huisartsLocatie"));
			return maakInzienHuisartsLocatiePanel(huisartsLocatie, false);
		case KoppelHuisarts:
		case ExtraHuisarts:
			if (selectedHuisartsLocatie == null)
			{
				return maakZoekHuisartsLocatiePanel();
			}
			else
			{
				return maakInzienHuisartsLocatiePanel(selectedHuisartsLocatie, true);
			}
		default:
			throw new IllegalStateException();
		}
	}

	private CervixHuisartsLocatiePanel maakInzienHuisartsLocatiePanel(IModel<CervixHuisartsLocatie> huisartsLocatie, boolean magAanpassen)
	{
		CervixHuisartsLocatiePanel result = new CervixHuisartsLocatiePanel("huisartsLocatieContainer", huisartsLocatie, magAanpassen)
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void wijzigHuisartsLocatie(AjaxRequestTarget target)
			{
				selectedHuisartsLocatie = null;
				CervixHuisartsberichtDetailsPanel.this.refreshLocatiePanel(target);
			}
		};
		result.setOutputMarkupId(true);
		return result;
	}

	private CervixZoekHuisartsLocatiePanel maakZoekHuisartsLocatiePanel()
	{
		CervixZoekHuisartsLocatiePanel result = new CervixZoekHuisartsLocatiePanel("huisartsLocatieContainer")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void setHuisartsLocatie(AjaxRequestTarget target, CervixHuisartsLocatie huisartsLocatie)
			{
				CervixHuisartsBericht huisartsBericht = getHuisartsBericht();
				if (panelState == PanelState.ExtraHuisarts && huisartsLocatie.equals(huisartsBericht.getHuisartsLocatie()))
				{
					ScreenitSession.get().error(getString("huisartsbericht.extralocatie.zelfde.als.origineel"));
				}
				else
				{
					selectedHuisartsLocatie = ModelUtil.ccModel(huisartsLocatie);
				}
				CervixHuisartsberichtDetailsPanel.this.refreshLocatiePanel(target);
			}

			@Override
			protected boolean toonAdresVelden()
			{
				return true;
			}
		};
		result.setOutputMarkupId(true);
		return result;
	}

	private void refreshLocatiePanel(AjaxRequestTarget target)
	{
		Label newZorgmailLabel = maakZorgmailLabel();
		zorgmailLabel.replaceWith(newZorgmailLabel);
		zorgmailLabel = newZorgmailLabel;
		target.add(zorgmailLabel);

		WebMarkupContainer newHuisartsContainer = maakHuisartsLocatieContainer();
		huisartsLocatiePanel.replaceWith(newHuisartsContainer);
		huisartsLocatiePanel = newHuisartsContainer;
		target.add(huisartsLocatiePanel);
		refreshButtonVisibility(target);
	}

	private Label maakZorgmailLabel()
	{
		String labelId = "huisartsBericht.huisartsLocatie.zorgmailklantnummer";
		Label result;
		if (panelState == PanelState.Normaal)
		{
			result = new Label(labelId);
		}
		else if (selectedHuisartsLocatie != null)
		{
			result = new Label(labelId, getSelectedHuisartsLocatie().getZorgmailklantnummer());
		}
		else
		{
			result = new Label(labelId, "");
		}
		result.setOutputMarkupId(true);
		return result;
	}

	private void refreshButtonVisibility(AjaxRequestTarget target)
	{
		standardButton.setVisible(isStandardButtonVisible());
		extraHuisartsButton.setVisible(isExtraHuisartsButtonVisible());
		target.add(standardButton);
		target.add(extraHuisartsButton);
	}

	private boolean isStandardButtonVisible()
	{
		return (panelState == PanelState.Normaal && magOpnieuwVersturen())
			|| (panelState == PanelState.KoppelHuisarts && selectedHuisartsLocatie != null);
	}

	private boolean isExtraHuisartsButtonVisible()
	{
		return (panelState == PanelState.Normaal && magOpnieuwVersturen())
			|| (panelState == PanelState.ExtraHuisarts && selectedHuisartsLocatie != null);
	}

	private boolean magOpnieuwVersturen()
	{
		return berichtVerstuurbaar() && heeftRechtOpnieuwVersturen();
	}

	private boolean berichtVerstuurbaar()
	{
		switch (getHuisartsBericht().getStatus())
		{
		case VERSTUREN_MISLUKT:
		case VERSTUURD:
		case OPNIEUW_VERSTUURD:
		case OPNIEUW_VERSTUREN_MISLUKT:
			return true;
		default:
			return false;
		}
	}

	private boolean heeftRechtOpnieuwVersturen()
	{
		return ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_HUISARTSBERICHT_OPNIEUW_VERZENDEN, Actie.AANPASSEN,
			getHuisartsBericht().getClient());
	}

	@Override
	protected void addButton(String id, GebeurtenisPopupBasePanel parent)
	{
		standardButton = new IndicatingAjaxLink<Void>(id)
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				CervixHuisartsBericht huisartsBericht = getHuisartsBericht();
				if (panelState == PanelState.KoppelHuisarts && huisartsBericht.getStatus() == CervixHuisartsBerichtStatus.HUISARTS_ONBEKEND)
				{
					koppelHuisartsEtc(huisartsBericht);
				}
				verstuurHuisartsBericht(huisartsBericht);
				setResponsePageClientDossier();
			}
		};

		standardButton.add(new Label("label", getStandardButtonText()));
		standardButton.setVisible(isStandardButtonVisible());
		standardButton.setOutputMarkupId(true);
		standardButton.setOutputMarkupPlaceholderTag(true);
		parent.add(standardButton);
	}

	private String getStandardButtonText()
	{
		if (panelState == PanelState.KoppelHuisarts)
		{
			return getString("huisartsbericht.huisarts.koppelen");
		}
		else if (getHuisartsBericht().getStatus() == CervixHuisartsBerichtStatus.VERSTUREN_MISLUKT)
		{
			return getString("huisartsbericht.versturen");
		}
		else
		{
			return getString("huisartsbericht.opnieuw.versturen");
		}
	}

	private void koppelHuisartsEtc(CervixHuisartsBericht huisartsBericht)
	{
		huisartsBericht.setHuisartsLocatie(getSelectedHuisartsLocatie());
		cervixVerrichtingFactory.maakHuisartsVerrichting(huisartsBericht.getUitstrijkje(), currentDateSupplier.getDate(), getSelectedHuisartsLocatie());
		huisartsBericht.setStatus(CervixHuisartsBerichtStatus.AANGEMAAKT);
		hibernateService.saveOrUpdate(huisartsBericht);
	}

	private void verstuurHuisartsBericht(CervixHuisartsBericht huisartsBericht)
	{
		ediService.verstuurMedVry(huisartsBericht, ScreenitSession.get().getLoggedInAccount());

		switch (huisartsBericht.getStatus())
		{
		case VERSTUURD:
			ScreenitSession.get().info(getString("huisartsbericht.verstuurd"));
			break;
		case VERSTUREN_MISLUKT:
			ScreenitSession.get().error(getString("huisartsbericht.versturen.mislukt"));
			break;
		case OPNIEUW_VERSTUURD:
			ScreenitSession.get().info(getString("huisartsbericht.opnieuw.verstuurd"));
			break;
		case OPNIEUW_VERSTUREN_MISLUKT:
			ScreenitSession.get().error(getString("huisartsbericht.opnieuw.versturen.mislukt"));
			break;
		case KLANTNUMMER_NIET_GEVERIFIEERD:
			ScreenitSession.get().error(getString("huisartsbericht.opnieuw.versturen.niet.mogelijk.klantnummer.niet.geverifieerd"));
			break;
		default:
			throw new IllegalStateException();
		}
	}

	@Override
	protected void addExtraButton(String id, GebeurtenisPopupBasePanel parent)
	{
		extraHuisartsButton = new ConfirmingIndicatingAjaxLink<Void>(id, dialog, "question.opnieuwversturen")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				if (panelState != PanelState.ExtraHuisarts)
				{
					panelState = PanelState.ExtraHuisarts;
					refreshLocatiePanel(target);
				}
				else
				{
					verstuurHuisartsBerichtNaarExtraLocatie(getHuisartsBericht(), getSelectedHuisartsLocatie());
					setResponsePageClientDossier();
				}
			}

			@Override
			protected IModel<String> getContentStringModel()
			{
				String formatted = String.format(super.getContentStringModel().getObject(), getSelectedHuisartsLocatie().getNaam(),
					getHuisartsBericht().getHuisartsLocatie().getNaam());
				return Model.of(formatted);
			}

			@Override
			protected boolean skipConfirmation()
			{
				return selectedHuisartsLocatie == null;
			}

		};

		extraHuisartsButton.setVisible(isExtraHuisartsButtonVisible());
		extraHuisartsButton.setOutputMarkupId(true);
		extraHuisartsButton.setOutputMarkupPlaceholderTag(true);
		extraHuisartsButton.add(new Label("label", getString("huisartsbericht.versturen.extra.huisarts")));
		parent.add(extraHuisartsButton);
	}

	private CervixHuisartsLocatie getSelectedHuisartsLocatie()
	{
		return ModelUtil.nullSafeGet(selectedHuisartsLocatie);
	}

	private void verstuurHuisartsBerichtNaarExtraLocatie(CervixHuisartsBericht huisartsBericht, CervixHuisartsLocatie locatie)
	{
		CervixEdiVerstuurStatus status = ediService.verstuurMedVryNaarExtraHuisartsLocatie(huisartsBericht, locatie, ScreenitSession.get().getLoggedInAccount());

		switch (status)
		{
		case VERSTUURD:
			ScreenitSession.get().info(getString("huisartsbericht.opnieuw.verstuurd"));
			break;
		case VERSTUREN_MISLUKT:
			ScreenitSession.get().error(getString("huisartsbericht.opnieuw.versturen.mislukt"));
			break;
		case KLANTNUMMER_NIET_GEVERIFIEERD:
			ScreenitSession.get().error(getString("huisartsbericht.opnieuw.versturen.niet.mogelijk.klantnummer.niet.geverifieerd"));
			break;
		}
	}

	private void setResponsePageClientDossier()
	{
		setResponsePage(new ClientDossierPage(ModelUtil.sModel(getHuisartsBericht().getClient())));
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(selectedHuisartsLocatie);
	}

	private CervixHuisartsBericht getHuisartsBericht()
	{
		return getModelObject().getHuisartsBericht();
	}

}
